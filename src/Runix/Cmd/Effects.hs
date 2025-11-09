{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Runix.Cmd.Effects where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Polysemy
import qualified System.Process as Process
import System.Exit (ExitCode(..))

-- | Result from command execution
data CmdOutput = CmdOutput
  { exitCode :: Int
  , stdout :: Text
  , stderr :: Text
  } deriving (Show, Eq)

-- | Cmd effect for executing single commands with arguments
-- This does not evaluate shell syntax (pipes, redirects, etc.)
-- It directly executes a command with its arguments
data Cmd (m :: Type -> Type) a where
    -- Execute a command with arguments
    CmdExec :: FilePath -> [String] -> Cmd m CmdOutput

makeSem ''Cmd

-- | IO interpreter for Cmd effect
cmdIO :: Member (Embed IO) r => Sem (Cmd : r) a -> Sem r a
cmdIO = interpret $ \case
    CmdExec prog args -> embed $ runCommand prog args
  where
    runCommand :: FilePath -> [String] -> IO CmdOutput
    runCommand prog args = do
        (exitCode, stdout, stderr) <- Process.readProcessWithExitCode prog args ""
        let code = case exitCode of
                ExitSuccess -> 0
                ExitFailure c -> c
        return $ CmdOutput code (T.pack stdout) (T.pack stderr)
