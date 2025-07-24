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


module Runix.Effects where
import Polysemy
import Polysemy.Fail
import Data.Kind (Type)
import Prelude hiding (readFile, writeFile)
import System.FilePath
import Data.List (isPrefixOf)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)


-- Effects
data FileSystem (m :: Type -> Type) a where
    ReadFile :: FilePath -> FileSystem m String
    WriteFile :: FilePath -> String -> FileSystem m ()

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
    headers :: [String],
    body :: Maybe ByteString
}

data HTTPResponse = HTTPResponse {
    code :: Int,
    headers :: [String],
    body :: ByteString
}

data HTTP (m :: Type -> Type) a where
    HttpRequest :: HTTPRequest -> HTTP m HTTPResponse
makeSem ''HTTP

newtype Endpoint = Endpoint String
newtype RestData a = RestData a
newtype RestResponse a = RestResponse a

data RestAPI (m :: Type -> Type) a where
    RestGet :: (FromJSON s) => Endpoint -> RestAPI m s
    RestPost :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI m s
    RestPut :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI m s
    RestDelete :: (FromJSON s) => Endpoint -> RestAPI m s
    RestPatch :: (ToJSON r, FromJSON s) => Endpoint -> r -> RestAPI m s
    RestCustom :: (ToJSON r, FromJSON s) => String -> Endpoint -> Maybe r -> RestAPI m s

makeSem ''RestAPI

data Logging (m :: Type -> Type) a where
    Info :: String -> Logging m ()
    Warning :: String -> Logging m ()
    Error :: String -> Logging m ()
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
