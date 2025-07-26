{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Runix.Runner (runUntrusted) where
-- Standard libraries
import Prelude hiding (readFile, writeFile, error)
import System.Environment (lookupEnv)

-- Polysemy libraries
import Polysemy
import Polysemy.Fail
import Polysemy.Error

-- Local modules
import Runix.Effects
import qualified Runix.Compiler as Compiler
import qualified Runix.Openrouter as Openrouter

-- External libraries
import Network.HTTP.Simple
import qualified Control.Monad.Catch as CMC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.String (fromString)
import Network.HTTP.Client.Conduit (RequestBody(RequestBodyLBS))
import GHC.Stack
import Data.List (intercalate)

-- Engine
type SafeEffects = [FileSystem, HTTP, CompileTask, Logging, LLM]

filesystemIO :: Members [Embed IO, Logging] r => Sem (FileSystem : r) a -> Sem r a
filesystemIO = interpret $ \case
    ReadFile p -> do
        info $ "reading file: " <> fromString p
        embed $ BL.readFile p
    WriteFile p d -> do
        info $ "writing file: " <> fromString p
        embed $ BL.writeFile p d




-- HTTP interpreter with header support
httpIO :: Members [Fail, Logging, Embed IO] r => Sem (HTTP : r) a -> Sem r a
httpIO = interpret $ \case
    HttpRequest request -> do
        let parsed = CMC.catch (parseRequest request.uri) Left
        req <- case parsed of
            Right r -> return r
            Left e -> fail $
                "error parsing uri: " <> request.uri <> "\n" <> show e
        let hdrs = map (\(hn, hv) -> (fromString hn, fromString hv)) request.headers
        info $ "rest: setting headers: " <> fromString (show hdrs)
        let hr :: Request =
                setRequestMethod (fromString request.method) .
                setRequestHeaders hdrs .
                case request.body of
                    Just b -> setRequestBody (RequestBodyLBS b)
                    Nothing -> Prelude.id
                $ req
        info $ "rest: sending request: " <> fromString (show hr)
        info $ "rest: with data: " <> fromString (show request.body)
        resp <- httpLBS hr
        return $ HTTPResponse
            { code = getResponseStatusCode resp
            , headers = map (\(hn, b) -> (show hn, show b)) $ getResponseHeaders resp -- FIXME
            , body = getResponseBody resp
            }

secretEnv :: Members [Fail, Embed IO] r => (String -> s) -> String -> Sem (Secret s :r) a -> Sem r a
secretEnv gensecret envname = interpret $ \case
    GetSecret -> do
        mk <- embed $ lookupEnv envname
        case mk of
            Nothing -> fail $ "secretEnv: ENV " <> envname <> " is unset"
            Just key -> pure $ gensecret key


openrouter :: Members [Embed IO, Fail, HTTP] r => Sem (LLM : RestAPI Openrouter.Openrouter : r) a -> Sem r a
openrouter = secretEnv Openrouter.OpenrouterKey "OPENROUTER_API" . Openrouter.openrouterapi . Openrouter.llmOpenrouter "deepseek/deepseek-chat-v3-0324:free"



-- Reinterpreter for HTTP with header support
withHeaders :: Members [Fail, Logging, HTTP] r => (HTTPRequest -> HTTPRequest) -> Sem r a -> Sem r a
withHeaders modifyRequest = intercept $ \case
    HttpRequest request -> do
        info "intercepted request"
        httpRequest (modifyRequest request)

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
            f@CompileFail {} -> do
                warning "compilation failure"
                info $ T.pack $ show f.compileWarnings
                warning $ T.pack $ show f.compileErrors
                return f
            s@CompileSuccess {} -> do
                info "compilation success"
                info $ T.pack $ show s.compileWarnings
                return s


loggingIO :: (HasCallStack, Member (Embed IO) r) => Sem (Logging : r) a -> Sem r a
loggingIO = interpret $ \v -> do
    case v of
        Info cs m -> embed $ putStrLn $ "info: " <> l cs m
        Warning cs m -> embed $ putStrLn $ "warn: " <> l cs m
        Error cs m -> embed $ putStrLn $ " err: " <> l cs m
    where
        l cs m = funname (getCallStack cs) <> T.unpack m
        funname (_:f:frames) = (intercalate "." . reverse . map fst) (f:frames) <> ": "
        funname _ = ""

loggingNull :: Sem (Logging : r) a -> Sem r a
loggingNull = interpret $ \case
    Info _ _ -> pure ()
    Warning _ _ -> pure ()
    Error _ _ -> pure ()

failLog :: Members [Logging, Error String] r => Sem (Fail : r) a -> Sem r a
failLog = interpret $ \(Fail e) -> error (T.pack e) >> throw e

runUntrusted :: HasCallStack => (forall r . Members SafeEffects r => Sem r a) -> IO (Either String a)
runUntrusted = runM . runError . loggingIO . failLog . httpIO . filesystemIO. openrouter .  compileTaskIO
