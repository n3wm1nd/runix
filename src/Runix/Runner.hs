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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Runix.Runner (runUntrusted, SafeEffects, filesystemIO, httpIO, httpIO_, withRequestTimeout, secretEnv, loggingIO, failLog, Coding) where

-- Standard libraries
import Prelude hiding (readFile, writeFile, error)
import System.Environment (lookupEnv)
import qualified System.Directory

-- Polysemy libraries
import Polysemy
import Polysemy.Fail
import Polysemy.Error

-- Local modules
import Runix.FileSystem.Effects
import Runix.HTTP.Effects
import Runix.Logging.Effects
import qualified Runix.Compiler.Compiler as Compiler
import Runix.LLM.Interpreter (OpenRouter(..), GenericModel(..), interpretOpenRouter)

-- External libraries
import Network.HTTP.Simple
import qualified Control.Monad.Catch as CMC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.String (fromString)
import Network.HTTP.Client.Conduit (RequestBody(RequestBodyLBS), responseTimeoutMicro)
import GHC.Stack
import Data.List (intercalate)
import Runix.Compiler.Effects
import Runix.LLM.Effects
import Runix.Secret.Effects (Secret(..), runSecret)


-- Capability marker typeclass
class Coding model

instance Coding GenericModel

-- Engine - Generic over provider and model
type SafeEffects provider model = [FileSystem, HTTP, CompileTask, Logging, LLM provider model]

filesystemIO :: HasCallStack => Members [Embed IO, Logging] r => Sem (FileSystem : r) a -> Sem r a
filesystemIO = interpret $ \case
    ReadFile p -> do
        info $ "reading file: " <> fromString p
        embed $ BL.readFile p
    WriteFile p d -> do
        info $ "writing file: " <> fromString p
        embed $ BL.writeFile p d
    ListFiles p -> do
        info $ "listing files: " <> fromString p
        embed $ System.Directory.listDirectory p
    FileExists p -> do
        info $ "checking file exists: " <> fromString p
        embed $ System.Directory.doesFileExist p
    IsDirectory p -> do
        info $ "checking is directory: " <> fromString p
        embed $ System.Directory.doesDirectoryExist p




-- HTTP interpreter with header support
httpIO :: HasCallStack => Members [Fail, Logging, Embed IO] r => (Request -> Request) -> Sem (HTTP : r) a -> Sem r a
httpIO requestTransform = interpret $ \case
    HttpRequest request -> do
        parsed <- embed $ CMC.try (parseRequest request.uri)
        req <- case parsed of
            Right r -> return r
            Left (e :: CMC.SomeException) -> fail $
                "error parsing uri: " <> request.uri <> "\n" <> show e
        let hdrs = map (\(hn, hv) -> (fromString hn, fromString hv)) request.headers
        -- info $ "setting headers: " <> fromString (show hdrs)
        let hr :: Request =
                setRequestMethod (fromString request.method) .
                setRequestHeaders hdrs .
                requestTransform .
                case request.body of
                    Just b -> setRequestBody (RequestBodyLBS b)
                    Nothing -> Prelude.id
                $ req
        info $ fromString request.method <> " " <> fromString request.uri
        -- info $ "with data: " <> fromString (show request.body)
        resp <- httpLBS hr
        return $ HTTPResponse
            { code = getResponseStatusCode resp
            , headers = map (\(hn, b) -> (show hn, show b)) $ getResponseHeaders resp -- FIXME
            , body = getResponseBody resp
            }

-- Convenience function with default behavior (no request transformation)
httpIO_ :: HasCallStack => Members [Fail, Logging, Embed IO] r => Sem (HTTP : r) a -> Sem r a
httpIO_ = httpIO Prelude.id

-- Helper function to set request timeout in seconds
withRequestTimeout :: Int -> Request -> Request
withRequestTimeout seconds = setRequestResponseTimeout (responseTimeoutMicro (seconds * 1000000))

secretEnv :: Members [Fail, Embed IO] r => (String -> s) -> String -> Sem (Secret s :r) a -> Sem r a
secretEnv gensecret envname = interpret $ \case
    GetSecret -> do
        mk <- embed $ lookupEnv envname
        case mk of
            Nothing -> fail $ "secretEnv: ENV " <> envname <> " is unset"
            Just key -> pure $ gensecret key


-- OpenRouter interpreter using environment variable via Secret effect
openrouter :: Members [Embed IO, Fail, HTTP] r => Sem (LLM OpenRouter GenericModel : r) a -> Sem r a
openrouter action = do
    apiKey <- embed $ lookupEnv "OPENROUTER_API_KEY"
    case apiKey of
        Nothing -> fail "OPENROUTER_API_KEY environment variable not set"
        Just key ->
            runSecret (pure key)
            . interpretOpenRouter OpenRouter (GenericModel "deepseek/deepseek-chat-v3-0324:free")
            . raiseUnder
            $ action


-- Reinterpreter for HTTP with header support
-- NOTE: Currently unused, but kept for future use
_withHeaders :: Members [Fail, Logging, HTTP] r => (HTTPRequest -> HTTPRequest) -> Sem r a -> Sem r a
_withHeaders modifyRequest = intercept $ \case
    HttpRequest request -> do
        info "intercepted request"
        httpRequest (modifyRequest request)

-- Example usage of withHeaders for setting authentication tokens:
-- 
-- authenticatedRequest :: Members [HTTP, RestAPI] r => Sem (HTTP : RestAPI : r) a -> Sem (HTTP : RestAPI : r) a
-- authenticatedRequest = withHeaders $ \req -> req { headers = ("Authorization", "Bearer token123") : headers req }


compileTaskIO :: HasCallStack => Members [ Embed IO, Logging, FileSystem ] r => Sem (CompileTask : r) a -> Sem r a
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


loggingIO :: HasCallStack => Member (Embed IO) r => Sem (Logging : r) a -> Sem r a
loggingIO = interpret $ \v -> do
    case v of
        Info cs m -> embed $ putStrLn $ "info: " <> l cs m
        Warning cs m -> embed $ putStrLn $ "warn: " <> l cs m
        Error cs m -> embed $ putStrLn $ " err: " <> l cs m
    where
        l cs m = funname (getCallStack cs) <> T.unpack m
        funname (_:f:frames) = (intercalate "." . reverse . map fst) (f:frames) <> ": "
        funname _ = ""

-- NOTE: Currently unused, but kept for future use
_loggingNull :: Sem (Logging : r) a -> Sem r a
_loggingNull = interpret $ \case
    Info _ _ -> pure ()
    Warning _ _ -> pure ()
    Error _ _ -> pure ()

failLog :: Members [Logging, Error String] r => Sem (Fail : r) a -> Sem r a
failLog = interpret $ \(Fail e) -> error (T.pack e) >> throw e

runUntrusted :: HasCallStack => (forall r . Members (SafeEffects OpenRouter GenericModel) r => Sem r a) -> IO (Either String a)
runUntrusted = runM . runError . loggingIO . failLog . httpIO_ . filesystemIO. openrouter .  compileTaskIO

