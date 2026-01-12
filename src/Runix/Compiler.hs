{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Runix.Compiler where

import Polysemy
import Data.Kind (Type)

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

