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
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Char (chr)
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
    -- Execute a command with arguments in a specific working directory with stdin
    CmdExecWithStdin :: FilePath -> FilePath -> [String] -> ByteString -> Cmd m CmdOutput

-- | Execute command in a specific working directory with no stdin
cmdExecIn :: Member Cmd r => FilePath -> FilePath -> [String] -> Sem r CmdOutput
cmdExecIn cwd prog args = send (CmdExecWithStdin cwd prog args BS.empty)

-- | Execute command in current working directory with no stdin
cmdExec :: Member Cmd r => FilePath -> [String] -> Sem r CmdOutput
cmdExec prog args = cmdExecIn "." prog args

-- | Execute command with stdin in current working directory
cmdExecStdin :: Member Cmd r => FilePath -> [String] -> ByteString -> Sem r CmdOutput
cmdExecStdin prog args stdin = send (CmdExecWithStdin "." prog args stdin)

cmdIO :: Member (Embed IO) r => Sem (Cmd : r) a -> Sem r a
cmdIO = interpret $ \case
    CmdExecWithStdin cwd prog args stdinContent -> embed $ runCommandIn cwd prog args stdinContent
  where
    runCommandIn :: FilePath -> FilePath -> [String] -> ByteString -> IO CmdOutput
    runCommandIn cwd prog args stdinContent = do
        let process = (Process.proc prog args) { Process.cwd = Just cwd }
            -- Convert ByteString to String - each Word8 becomes a Char (8-bit raw data)
            stdinString = map (chr . fromIntegral) (BS.unpack stdinContent)
        (exitCode, stdout, stderr) <- Process.readCreateProcessWithExitCode process stdinString
        let code = case exitCode of
                ExitSuccess -> 0
                ExitFailure c -> c
        return $ CmdOutput code (T.pack stdout) (T.pack stderr)
