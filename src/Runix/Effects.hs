{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}



module Runix.Effects where
import Polysemy
import Polysemy.Fail
import Data.Kind (Type)
import Prelude hiding (readFile, writeFile)
import System.FilePath
import Data.List (isPrefixOf)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text hiding (isPrefixOf)


-- Effects
data FileSystem (m :: Type -> Type) a where
    ReadFile :: FilePath -> FileSystem m ByteString
    WriteFile :: FilePath -> ByteString -> FileSystem m ()

makeSem ''FileSystem

data AccessPermission = AllowAccess | ForbidAccess String

limitedAccess :: Members [FileSystem, Fail] r => (forall m x. FileSystem m x -> AccessPermission) -> Sem (FileSystem : r) a -> Sem (FileSystem : r) a
limitedAccess isAllowed = intercept $ \action -> case isAllowed action of
    AllowAccess -> case action of
        ReadFile f -> readFile f
        WriteFile f c -> writeFile f c

    ForbidAccess reason -> fail $ "not allowed: " ++ reason

limitSubpath :: (Member Fail r, Member FileSystem r) => FilePath -> Sem (FileSystem : r) a -> Sem (FileSystem : r) a
limitSubpath p = limitedAccess (\case
    ReadFile sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    WriteFile sp _c | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    _ -> ForbidAccess $ p ++ " is not in explicitly allowed path " ++ p
    )


data HTTPRequest = HTTPRequest {
    method :: String,
    uri :: String,
    headers :: [(String, String)],
    body :: Maybe ByteString
} deriving (Show)

data HTTPResponse = HTTPResponse {
    code :: Int,
    headers :: [(String, String)],
    body :: ByteString
} deriving (Show)

data HTTP (m :: Type -> Type) a where
    HttpRequest :: HTTPRequest -> HTTP m HTTPResponse
makeSem ''HTTP

newtype Endpoint = Endpoint String
newtype RestData a = RestData a
newtype RestResponse a = RestResponse a

data RestAPI p (m :: Type -> Type) a where
    RestGet :: (FromJSON s) => Endpoint -> RestAPI p m s
    RestPost :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI p m s
    RestPut :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI p m s
    RestDelete :: (FromJSON s) => Endpoint -> RestAPI p m s
    RestPatch :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI p m s
    RestCustom :: (ToJSON r, FromJSON s) => String -> Endpoint -> Maybe r -> RestAPI p m s
makeSem ''RestAPI
class RestEndpoint p where
    apiroot :: p -> String
    authheaders :: p -> [(String, String)]

data LLM (m :: Type -> Type) a where
    AskLLM :: Text -> LLM m Text
makeSem ''LLM


data Logging (m :: Type -> Type) a where
    Info :: Text -> Logging m ()
    Warning :: Text -> Logging m ()
    Error :: Text -> Logging m ()
makeSem ''Logging

data (Config c) (m :: Type -> Type ) a where
    GetConfig :: (Config c) m c
makeSem ''Config

runConfig ::  c -> Sem (Config c:r) a -> Sem r a
runConfig con = interpret $ \case
    GetConfig -> pure con

data (Secret s) (m :: Type -> Type) a where
    GetSecret :: (Secret s) m s
makeSem ''Secret

runSecret :: Sem r s -> Sem (Secret s:r) a -> Sem r a
runSecret s = interpret $ \case
    GetSecret -> s

data HaskellModule = HaskellSource {modulename :: String, sourcecode :: String}
  deriving (Show, Eq)

newtype HaskellPackage = HaskellPackage {packagename :: String}

type ProjectName = String

type ProjectDescription = String

data HaskellProject = HaskellProject
  { name :: ProjectName,
    description :: ProjectDescription,
    main :: HaskellModule,
    modules :: [HaskellModule],
    other_modules :: [HaskellModule],
    dependencies :: [HaskellPackage]
  }

-- Enhanced compilation result with detailed error information
data CompileResult = CompileSuccess
    { compileWarnings :: [CompilationError]
    } |
    CompileFail {
        compileErrors :: [CompilationError]
    ,   compilewarnings :: [CompilationError]
    }
    deriving (Show, Eq)

-- Detailed compilation error information
data CompilationError = CompilationError
    { file :: FilePath
    , line :: Int
    , column :: Maybe Int
    , message :: String
    } deriving (Show, Eq)

data CompileTask (m :: Type -> Type) a where
    CompileTask :: HaskellProject -> CompileTask m CompileResult

makeSem ''CompileTask


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