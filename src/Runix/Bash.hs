{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Runix.Bash where

import Data.Kind (Type)
import Data.Text (Text)
import Data.String (fromString)
import Polysemy
import GHC.Stack
import Runix.Cmd (Cmd, cmdExec)
import qualified Runix.Cmd as CmdE
import Runix.Logging (Logging, info)

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

-- | Interpreter for Bash effect using Cmd
bashIO :: HasCallStack => Members [Cmd, Logging] r => Sem (Bash : r) a -> Sem r a
bashIO = interpret $ \case
    BashExec cmd -> do
        info $ fromString "bash exec: " <> fromString cmd
        result <- cmdExec "/bin/bash" ["-c", cmd]
        return $ BashOutput (CmdE.exitCode result) (CmdE.stdout result) (CmdE.stderr result)
