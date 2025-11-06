{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Runix.Grep.Effects where

import Data.Kind (Type)
import Data.Text (Text)
import Polysemy

-- | Grep search result
data GrepMatch = GrepMatch
  { matchFile :: FilePath
  , matchLine :: Int
  , matchText :: Text
  } deriving (Show, Eq)

-- | Grep effect for searching file contents
data Grep (m :: Type -> Type) a where
    -- Search for pattern in files under basePath
    GrepSearch :: FilePath -> String -> Grep m [GrepMatch]

makeSem ''Grep
