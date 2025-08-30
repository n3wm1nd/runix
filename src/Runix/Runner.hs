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


module Runix.Runner (runUntrusted, cliRunner, Task(..), SafeEffects) where
-- Standard libraries
import Prelude hiding (readFile, writeFile, error)
import System.Environment (lookupEnv, getArgs)
import qualified System.Directory

-- Polysemy libraries
import Polysemy
import Polysemy.Fail
import Polysemy.Error

-- Local modules
import Runix.FileSystem.Effects
import Runix.HTTP.Effects
import Runix.Logging.Effects
import Runix.RestAPI.Effects
import qualified Runix.Compiler.Compiler as Compiler
import qualified Runix.LLM.Openrouter as Openrouter

-- External libraries
import Network.HTTP.Simple
import qualified Control.Monad.Catch as CMC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.String (fromString)
import Network.HTTP.Client.Conduit (RequestBody(RequestBodyLBS))
import GHC.Stack
import Data.List (intercalate, find, uncons)
import Data.Aeson (FromJSON, ToJSON, decode, encode, eitherDecode)
import System.Exit (exitFailure)
import System.IO (stderr)
import GHC.IO.Handle (hPutStr)
import Runix.Compiler.Effects
import Runix.LLM.Effects
import Runix.Secret.Effects


data FallbackModel = FallbackModel
instance LLMModel FallbackModel where
    modelidentifier _ = "deepseek/deepseek-chat-v3-0324:free"

-- Engine
type SafeEffects = [FileSystem, HTTP, CompileTask, Logging, LLM FallbackModel]

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
httpIO :: HasCallStack => Members [Fail, Logging, Embed IO] r => Sem (HTTP : r) a -> Sem r a
httpIO = interpret $ \case
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

secretEnv :: Members [Fail, Embed IO] r => (String -> s) -> String -> Sem (Secret s :r) a -> Sem r a
secretEnv gensecret envname = interpret $ \case
    GetSecret -> do
        mk <- embed $ lookupEnv envname
        case mk of
            Nothing -> fail $ "secretEnv: ENV " <> envname <> " is unset"
            Just key -> pure $ gensecret key


openrouter :: Members [Embed IO, Fail, HTTP] r => Sem (LLM FallbackModel : RestAPI Openrouter.Openrouter : r) a -> Sem r a
openrouter = secretEnv Openrouter.OpenrouterKey "OPENROUTER_API" . Openrouter.openrouterapi . Openrouter.llmOpenrouter FallbackModel



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

loggingNull :: Sem (Logging : r) a -> Sem r a
loggingNull = interpret $ \case
    Info _ _ -> pure ()
    Warning _ _ -> pure ()
    Error _ _ -> pure ()

failLog :: Members [Logging, Error String] r => Sem (Fail : r) a -> Sem r a
failLog = interpret $ \(Fail e) -> error (T.pack e) >> throw e

runUntrusted :: HasCallStack => (forall r . Members SafeEffects r => Sem r a) -> IO (Either String a)
runUntrusted = runM . runError . loggingIO . failLog . httpIO . filesystemIO. openrouter .  compileTaskIO

-- Task representation
data Task where
  Task :: (FromJSON a, ToJSON b) =>
    { taskName :: String
    , taskFunc :: a -> (forall r . Members SafeEffects r => Sem r b)
    } -> Task

-- CLI Runner implementation
cliRunner :: [Task] -> IO ()
cliRunner tasks = do
  result <- runM . runError $ do
    args <- embed getArgs
    tName <- note "no taskname given" $ fmap fst (uncons args)

    -- Find the task by name
    Task _name taskFn <-
      note ("Error: Task '" <> tName <> "' not found") $
      find ( (== tName) . taskName) tasks

    input <- embed BL.getContents >>= fromEither . eitherDecode

    output <- embed $ runUntrusted (taskFn input)
    fromEither $ fmap encode output
  case result of
    Right o -> BL.putStr o
    Left e -> hPutStr stderr e >> exitFailure
