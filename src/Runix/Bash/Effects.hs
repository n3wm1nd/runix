{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Runix.Bash.Effects where

import Data.Kind (Type)
import Data.Text (Text)
import Polysemy

-- | Result from bash command execution
data BashOutput = BashOutput
  { exitCode :: Int
  , stdout :: Text
  , stderr :: Text
  } deriving (Show, Eq)

-- | Bash effect for executing shell commands
data Bash (m :: Type -> Type) a where
    -- Execute a bash command
    BashExec :: String -> Bash m BashOutput

makeSem ''Bash
