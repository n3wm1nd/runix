{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}


module Runix.Runner (runUntrusted) where
import Prelude hiding (readFile, writeFile, error)
import qualified Prelude

import Polysemy
import Polysemy.Fail
import Polysemy.Error

import Runix.Effects
import qualified Runix.Compiler as Compiler
import Data.Either (fromRight)
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Simple
import qualified Control.Monad.Catch as CMC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import GHC.Generics (Generic)
import Distribution.TestSuite (TestInstance(run))
import System.Environment (getEnv)

-- Engine
type SafeEffects = [FileSystem, HTTP, RestAPI, CompileTask]

filesystemIO :: Members [Embed IO, Logging] r => Sem (FileSystem : r) a -> Sem r a
filesystemIO = interpret $ \case
    ReadFile p -> do
        info $ "reading file: " <> T.pack p
        embed $ BL.readFile p
    WriteFile p d -> do
        info $ "writing file: " <> T.pack p
        embed $ BL.writeFile p d

restapiIO :: Members [Embed IO, HTTP, Fail] r => Sem (RestAPI : r) a -> Sem r a
restapiIO = interpret $ \case
    RestGet e -> request "GET" e Nothing
    RestPost e d -> request "POST" e (Just $ encode d)
    RestPut e d -> request "PUT" e (Just $ encode d)
    RestDelete e -> request "DELETE" e Nothing
    RestPatch e d -> request "PATCH" e (Just $ encode d)
    RestCustom method e d -> request method e (fmap encode d)
  where
    request :: (FromJSON a, Members [Fail, HTTP] r) =>
        String -> Endpoint -> Maybe ByteString -> Sem r a
    request method (Endpoint endpoint) body =
        httpRequest (HTTPRequest
            { method = method
            , uri = endpoint
            , headers = []
            , body = body
            } ) >>= parseResponse
    parseResponse :: (FromJSON a, Member Fail r) =>
        HTTPResponse -> Sem r a
    parseResponse response =
        case decode response.body of
            Just result -> return result
            Nothing -> fail "Failed to parse JSON response"

-- HTTP interpreter with header support
httpIO :: Members [Fail, Embed IO] r => Sem (HTTP : r) a -> Sem r a
httpIO = interpret $ \case
    HttpRequest request -> do
        let parsed = CMC.catch (parseRequest request.uri) Left
        req <- case parsed of
            Right r -> return r
            Left e -> fail $
                "error parsing uri: " <> request.uri <> "\n" <> show e
        resp <- httpLBS req
        return $ HTTPResponse
            { code = getResponseStatusCode resp
            , headers = map (\(hn, b) -> show hn <> ": " <> show b) $ getResponseHeaders resp -- FIXME
            , body = getResponseBody resp
            }

data OpenrouterMessage = OpenrouterMessage {role :: String, content :: T.Text}
    deriving (Generic, ToJSON)
data OpenrouterQuery = OpenrouterQuery {model :: String, messages :: [OpenrouterMessage]}
    deriving (Generic, ToJSON)
newtype OpenrouterKey = OpenrouterKey String


llmOpenrouter :: Members [Fail, HTTP, RestAPI, Secret OpenrouterKey] r => Sem (LLM : r) a -> Sem r a
llmOpenrouter a = do
    OpenrouterKey key <- getSecret @OpenrouterKey
    withHeaders (settoken key) . llmOpenrouterReq $ a
    where
        settoken :: String -> HTTPRequest -> HTTPRequest
        settoken apikey r@HTTPRequest{headers} = r { headers = ("Authorization: Bearer " <> apikey) : headers }
        llmOpenrouterReq :: Members [Fail, RestAPI] r => Sem (LLM : r) a -> Sem r a
        llmOpenrouterReq = interpret $ \case
            AskLLM query -> restPost (Endpoint "https://openrouter.ai/api/v1/chat/completions") OpenrouterQuery {model="gqen/gwen3-coder:free", messages=[OpenrouterMessage "user" query]}

openrouterSecretEnv :: Member (Embed IO) r => Sem (Secret OpenrouterKey :r) a -> Sem r a
openrouterSecretEnv = interpret $ \case
    GetSecret -> fmap OpenrouterKey $ embed $ getEnv "OPENROUTER_API"

openrouter :: Members [Embed IO, Fail, HTTP, RestAPI] r => Sem (LLM : Secret OpenrouterKey : r) a -> Sem r a
openrouter = openrouterSecretEnv . llmOpenrouter

-- Reinterpreter for HTTP with header support
withHeaders :: Member HTTP r => (HTTPRequest -> HTTPRequest) -> Sem r a -> Sem r a
withHeaders modifyRequest = intercept $ \case
    HttpRequest request -> httpRequest (modifyRequest request)

-- Example usage of withHeaders for setting authentication tokens:
-- 
-- authenticatedRequest :: Members [HTTP, RestAPI] r => Sem (HTTP : RestAPI : r) a -> Sem (HTTP : RestAPI : r) a
-- authenticatedRequest = withHeaders $ \req -> req { headers = ("Authorization", "Bearer token123") : headers req }


compileTaskIO :: Members [ Embed IO, Logging, FileSystem ] r => Sem (CompileTask : r) a -> Sem r a
compileTaskIO = interpret $ \case
    CompileTask project -> do
        info $ "compiling haskell code: " <> T.pack project.name
        result <- embed $ Compiler.compile project
        case result of
            -- FIXME show errors and warnings in info/warning log
            f@CompileFail {} -> do
                warning "compilation failure"
                info $ T.pack $ show f.compileWarnings
                warning $ T.pack $ show f.compileErrors
                return f
            s@CompileSuccess {} -> do
                info "compilation success"
                info $ T.pack $ show s.compileWarnings
                return s


loggingIO :: Member (Embed IO) r => Sem (Logging : r) a -> Sem r a
loggingIO = interpret $ \case
    Info m -> embed $ putStrLn $ "info: " <> T.unpack m
    Warning m -> embed $ putStrLn $ "warn: " <> T.unpack m
    Error m -> embed $ putStrLn $ " err: " <> T.unpack m

loggingNull :: Sem (Logging : r) a -> Sem r a
loggingNull = interpret $ \case
    Info _ -> pure ()
    Warning _ -> pure ()
    Error _ -> pure ()

failLog :: Members [Logging, Error String] r => Sem (Fail : r) a -> Sem r a
failLog = interpret $ \(Fail e) -> error (T.pack e) >> throw e

runUntrusted :: (forall r . Members SafeEffects r => Sem r a) -> IO (Either String a)
runUntrusted = runM . runError . loggingIO . failLog . httpIO . filesystemIO. restapiIO. openrouter .  compileTaskIO

x :: Member FileSystem r => Sem r ()
x = do
    _ <- readFile "."
    return ()

rx :: IO ()
rx = do
    res <- runUntrusted x
    return $ fromRight () res
