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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}


module Runix.Runner (runUntrusted) where
-- Standard libraries
import Prelude hiding (readFile, writeFile, error)
import System.Environment (getEnv)

-- Polysemy libraries
import Polysemy
import Polysemy.Fail
import Polysemy.Error

-- Local modules
import Runix.Effects
import qualified Runix.Compiler as Compiler

-- External libraries
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Simple
import qualified Control.Monad.Catch as CMC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.String (fromString)
import Network.HTTP.Client.Conduit (RequestBody(RequestBodyLBS))
import Data.Maybe
import System.FilePath ((</>))

-- Engine
type SafeEffects = [FileSystem, HTTP, CompileTask, LLM]

filesystemIO :: Members [Embed IO, Logging] r => Sem (FileSystem : r) a -> Sem r a
filesystemIO = interpret $ \case
    ReadFile p -> do
        info $ "reading file: " <> T.pack p
        embed $ BL.readFile p
    WriteFile p d -> do
        info $ "writing file: " <> T.pack p
        embed $ BL.writeFile p d

class RestEndpoint p where
    apiroot :: p -> String
    authheaders :: p -> [(String, String)]


restapiHTTP :: (RestEndpoint p, Members [HTTP, Fail] r) => p -> Sem (RestAPI p : r) a -> Sem r a
restapiHTTP api = interpret $ \case
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
            , uri = apiroot api </> endpoint
            , headers = ("Content-Type", "application/json") : authheaders api
            , body = body
            } ) >>= parseResponse
    parseResponse :: (FromJSON a, Member Fail r) =>
        HTTPResponse -> Sem r a
    parseResponse response =
        case decode response.body of
            Just result -> return result
            Nothing -> fail $ "Failed to parse JSON response: " <> show response.body

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

data OpenrouterMessage = OpenrouterMessage {role :: String, content :: T.Text}
    deriving (Generic, ToJSON, FromJSON)
data OpenrouterQuery = OpenrouterQuery {model :: String, messages :: [OpenrouterMessage]}
    deriving (Generic, ToJSON, FromJSON)
newtype OpenrouterKey = OpenrouterKey String

data OpenrouterResponse = OpenrouterResponse {
    id :: String,
    model :: String,
    choices :: [OpenrouterChoice],
    usage :: OpenrouterUsage
    }
    deriving (Generic, ToJSON, FromJSON)
data OpenrouterUsage = OpenrouterUsage {
    prompt_tokens :: Int,
    completion_tokens :: Int,
    total_tokens :: Int
}
    deriving (Generic, ToJSON, FromJSON)

data OpenrouterChoice = OpenrouterChoice {
    finish_reason :: String,
    native_finish_reason :: String,
    message :: OpenrouterMessage
}
    deriving (Generic, ToJSON, FromJSON)


data Openrouter = Openrouter {
    apikey :: String
}

instance RestEndpoint Openrouter where
    apiroot _ = "https://openrouter.ai/api/v1/"
    authheaders a = [("Authorization", "Bearer " <> a.apikey)]

llmOpenrouter :: Members [Fail, HTTP, RestAPI Openrouter, Logging] r => String -> Sem (LLM : r) a -> Sem r a
llmOpenrouter model = interpret $ \case
    AskLLM query -> do
        resp :: OpenrouterResponse <- restPost 
            (Endpoint "chat/completions")
            OpenrouterQuery { model=model, messages=[OpenrouterMessage "user" query]}
        case resp.choices of
            c:_ -> return c.message.content
            [] -> fail "openrouter: no choices returned"

secretEnv :: Members [Fail, Embed IO] r => (String -> s) -> String -> Sem (Secret s :r) a -> Sem r a
secretEnv gensecret envname = interpret $ \case
    GetSecret -> do
        key <- embed $ getEnv envname
        if key == ""
            then fail $ "ENV " <> envname <> " is unset"
            else pure $ gensecret key

openrouter :: Members [Embed IO, Logging, Fail, HTTP] r => Sem (LLM : RestAPI Openrouter : r) a -> Sem r a
openrouter = secretEnv OpenrouterKey "OPENROUTER_API" . orouterapi . llmOpenrouter "deepseek/deepseek-chat-v3-0324:free"

orouterapi :: Members [Logging, Fail, HTTP] r => Sem (RestAPI Openrouter:r) a -> Sem (Secret OpenrouterKey:r) a
orouterapi a = do
    OpenrouterKey key <- getSecret
    restapiHTTP (Openrouter { apikey = key }) (raiseUnder a)



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
runUntrusted = runM . runError . loggingIO . failLog . httpIO . filesystemIO. openrouter .  compileTaskIO

x :: Member LLM r => Sem r T.Text
x = do
    askLLM "what is the answer to life, the universe, and everything?"

rx :: IO (Either String T.Text)
rx = do
    runUntrusted x