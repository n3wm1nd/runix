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
import GHC.Stack (HasCallStack, withFrozenCallStack, CallStack, callStack)


-- Effects
data FileSystem (m :: Type -> Type) a where
    ReadFile :: FilePath -> FileSystem m ByteString
    WriteFile :: FilePath -> ByteString -> FileSystem m ()
    ListFiles :: FilePath -> FileSystem m [FilePath]
    FileExists :: FilePath -> FileSystem m Bool
    IsDirectory :: FilePath -> FileSystem m Bool

makeSem ''FileSystem

data AccessPermission = AllowAccess | ForbidAccess String

limitedAccess :: Members [FileSystem, Fail] r => (forall m x. FileSystem m x -> AccessPermission) -> Sem (FileSystem : r) a -> Sem (FileSystem : r) a
limitedAccess isAllowed = intercept $ \action -> case isAllowed action of
    AllowAccess -> case action of
        ReadFile f -> readFile f
        WriteFile f c -> writeFile f c
        ListFiles f -> listFiles f
        FileExists f -> fileExists f
        IsDirectory f -> isDirectory f

    ForbidAccess reason -> fail $ "not allowed: " ++ reason

limitSubpath :: (Member Fail r, Member FileSystem r) => FilePath -> Sem (FileSystem : r) a -> Sem (FileSystem : r) a
limitSubpath p = limitedAccess (\case
    ReadFile sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    WriteFile sp _c | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    ListFiles sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    FileExists sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    IsDirectory sp | splitPath p `isPrefixOf` splitPath sp -> AllowAccess
    _ -> ForbidAccess $ p ++ " is not in explicitly allowed path " ++ p
    )

-- Chroot into a subdirectory by rewriting paths relative to the chroot path
chrootSubpath :: Member FileSystem r => FilePath -> Sem (FileSystem : r) a -> Sem (FileSystem : r) a
chrootSubpath chrootPath = intercept $ \case
    ReadFile f -> readFile (chrootPath </> f)
    WriteFile f c -> writeFile (chrootPath </> f) c
    ListFiles f -> do
        files <- listFiles (chrootPath </> f)
        -- Remove the chroot prefix from returned paths
        return $ fmap (makeRelative chrootPath) files
    FileExists f -> fileExists (chrootPath </> f)
    IsDirectory f -> isDirectory (chrootPath </> f)

-- Hide dotfiles (files starting with '.')
hideDotfiles :: Members [FileSystem, Fail] r => Sem (FileSystem : r) a -> Sem (FileSystem : r) a
hideDotfiles = intercept $ \case 
    ReadFile f | isDotfile f -> fail $ "Access to dotfile denied: " ++ f
                | otherwise -> readFile f
    WriteFile f c | isDotfile f -> fail $ "Access to dotfile denied: " ++ f
                  | otherwise -> writeFile f c
    ListFiles f -> do
        files <- listFiles f
        -- Filter out dotfiles from the listing
        return $ Prelude.filter (not . isDotfile . takeFileName) files
    FileExists f | isDotfile f -> return False  -- Hide existence of dotfiles
                 | otherwise -> fileExists f
    IsDirectory f | isDotfile f -> return False  -- Hide existence of dot directories
                  | otherwise -> isDirectory f
  where
    isDotfile :: FilePath -> Bool
    isDotfile path = case takeFileName path of
        ('.':_) -> True
        _ -> False


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
    Get :: (FromJSON s) => Endpoint -> RestAPI p m s
    Post :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI p m s
    Put :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI p m s
    Delete :: (FromJSON s) => Endpoint -> RestAPI p m s
    Patch :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI p m s
    Rest :: (ToJSON r, FromJSON s) => String -> Endpoint -> Maybe r -> RestAPI p m s
makeSem ''RestAPI
class RestEndpoint p where
    apiroot :: p -> String
    authheaders :: p -> [(String, String)]

data LLM (m :: Type -> Type) a where
    AskLLM :: Text -> LLM m Text
makeSem ''LLM


data Logging (m :: Type -> Type) a where
    Info :: HasCallStack => CallStack -> Text -> Logging m ()
    Warning :: HasCallStack => CallStack -> Text -> Logging m ()
    Error :: HasCallStack => CallStack -> Text -> Logging m ()
--makeSem ''Logging
info :: HasCallStack => Member Logging r => Text -> Sem r ()
info t = withFrozenCallStack $ send (Info callStack t) 
warning :: HasCallStack => Member Logging r => Text -> Sem r ()
warning t = withFrozenCallStack $ send (Warning callStack t)
error :: HasCallStack => Member Logging r => Text -> Sem r ()
error t = withFrozenCallStack $ send (Runix.Effects.Error callStack t)

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


restapiHTTP :: HasCallStack => (RestEndpoint p, Members [HTTP, Fail] r) => p -> Sem (RestAPI p : r) a -> Sem r a
restapiHTTP api = interpret $ \case
    Get e -> request "GET" e Nothing
    Post e d -> request "POST" e (Just $ encode d)
    Put e d -> request "PUT" e (Just $ encode d)
    Delete e -> request "DELETE" e Nothing
    Patch e d -> request "PATCH" e (Just $ encode d)
    Rest method e d -> request method e (fmap encode d)
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
