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


module Runix.Runner (runUntrusted) where
import Prelude hiding (readFile, writeFile, error)
import qualified Prelude

import Polysemy
import Polysemy.Fail
import Polysemy.Error

import Runix.Effects
import qualified Runix.Compiler as Compiler
import Data.Either (fromRight)
import Data.Aeson (encode, decode, FromJSON)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Simple
import qualified Control.Monad.Catch as CMC
import qualified Data.ByteString.Lazy as BL

-- Engine
type SafeEffects = [FileSystem, HTTP, RestAPI, CompileTask]

filesystemIO :: Members [Embed IO, Logging] r => Sem (FileSystem : r) a -> Sem r a
filesystemIO = interpret $ \case
    ReadFile p -> do
        info $ "reading file: " ++ p
        embed $ BL.readFile p
    WriteFile p d -> do
        info $ "writing file: " ++ p
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

-- Reinterpreter for HTTP with header support
withHeaders :: Member HTTP r => (HTTPRequest -> HTTPRequest) -> Sem (HTTP : r) a -> Sem (HTTP : r) a
withHeaders modifyRequest = reinterpret $ \case
    HttpRequest request -> httpRequest (modifyRequest request)

-- Example usage of withHeaders for setting authentication tokens:
-- 
-- authenticatedRequest :: Members [HTTP, RestAPI] r => Sem (HTTP : RestAPI : r) a -> Sem (HTTP : RestAPI : r) a
-- authenticatedRequest = withHeaders $ \req -> req { headers = ("Authorization", "Bearer token123") : headers req }


compileTaskIO :: Members [ Embed IO, Logging, FileSystem ] r => Sem (CompileTask : r) a -> Sem r a
compileTaskIO = interpret $ \case
    CompileTask project -> do
        info $ "compiling haskell code: " <> project.name
        result <- embed $ Compiler.compile project
        case result of
            -- FIXME show errors and warnings in info/warning log
            f@CompileFail {} -> do
                warning "compilation failure"
                info $ show f.compileWarnings
                warning $ show f.compileErrors
                return f
            s@CompileSuccess {} -> do
                info "compilation success"
                info $ show s.compileWarnings
                return s


loggingIO :: Member (Embed IO) r => Sem (Logging : r) a -> Sem r a
loggingIO = interpret $ \case
    Info m -> embed $ putStrLn $ "info: " ++ m
    Warning m -> embed $ putStrLn $ "warn: " ++ m
    Error m -> embed $ putStrLn $ " err: " ++ m

loggingNull :: Sem (Logging : r) a -> Sem r a
loggingNull = interpret $ \case
    Info _ -> pure ()
    Warning _ -> pure ()
    Error _ -> pure ()

failLog :: Members [Logging, Error String] r => Sem (Fail : r) a -> Sem r a
failLog = interpret $ \(Fail e) -> error e >> throw e

runUntrusted :: (forall r . Members SafeEffects r => Sem r a) -> IO (Either String a)
runUntrusted = runM . runError . loggingIO . failLog . httpIO . filesystemIO. restapiIO. compileTaskIO

x :: Member FileSystem r => Sem r ()
x = do
    _ <- readFile "."
    return ()

rx :: IO ()
rx = do
    res <- runUntrusted x
    return $ fromRight () res
