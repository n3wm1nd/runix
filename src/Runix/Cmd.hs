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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Runix.Cmd where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Polysemy
import qualified System.Process as Process
import System.Exit (ExitCode(..))

-- | Result from command execution
data CmdOutput = CmdOutput
  { exitCode :: Int
  , stdout :: Text
  , stderr :: Text
  } deriving (Show, Eq)

-- | Cmds effect for executing any command with arguments
-- This does not evaluate shell syntax (pipes, redirects, etc.)
-- It directly executes a command with its arguments
-- For constrained command execution, use Cmd (parametrized by command name)
data Cmds (m :: Type -> Type) a where
    -- Execute a command with arguments in a specific working directory with stdin
    CmdsExecWithStdin :: FilePath -> FilePath -> [String] -> ByteString -> Cmds m CmdOutput

-- | Execute command in a specific working directory with no stdin
cmdsExecIn :: Member Cmds r => FilePath -> FilePath -> [String] -> Sem r CmdOutput
cmdsExecIn cwd prog args = send (CmdsExecWithStdin cwd prog args BS.empty)

-- | Execute command in current working directory with no stdin
cmdsExec :: Member Cmds r => FilePath -> [String] -> Sem r CmdOutput
cmdsExec prog args = cmdsExecIn "." prog args

-- | Execute command with stdin in current working directory
cmdsExecStdin :: Member Cmds r => FilePath -> [String] -> ByteString -> Sem r CmdOutput
cmdsExecStdin prog args stdin = send (CmdsExecWithStdin "." prog args stdin)

cmdsIO :: Member (Embed IO) r => Sem (Cmds : r) a -> Sem r a
cmdsIO = interpret $ \case
    CmdsExecWithStdin cwd prog args stdinContent -> embed $ runCommandIn cwd prog args stdinContent
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

--------------------------------------------------------------------------------
-- Parametrized Single-Command Effect (Type-Level Command Restriction)
--------------------------------------------------------------------------------

-- | Parametrized effect for executing a single specific command using type-level strings
-- This provides type-level guarantees about which command can be executed
-- The interface is identical to Cmds, just with the command name fixed at the type level
--
-- Example usage:
--
-- @
-- myFunc :: Member (Cmd \"git\") r => Sem r ()
-- myFunc = do
--   output <- call @\"git\" [\"status\"]
--   -- Can only run git, not arbitrary commands
-- @
data Cmd (command :: Symbol) (m :: Type -> Type) a where
    -- | Call command with arguments in a specific working directory with stdin
    CmdExecWithStdin :: FilePath -> [String] -> ByteString -> Cmd command m CmdOutput

-- | Call command in a specific working directory with no stdin
callIn :: forall command r. Member (Cmd command) r => FilePath -> [String] -> Sem r CmdOutput
callIn cwd args = send @(Cmd command) (CmdExecWithStdin cwd args BS.empty)

-- | Call command in current working directory with no stdin
call :: forall command r. Member (Cmd command) r => [String] -> Sem r CmdOutput
call args = callIn @command "." args

-- | Call command with stdin in current working directory
callStdin :: forall command r. Member (Cmd command) r => [String] -> ByteString -> Sem r CmdOutput
callStdin args stdinContent = send @(Cmd command) (CmdExecWithStdin "." args stdinContent)

-- | Interpret Cmd on top of Cmds effect
-- The command name is extracted from the type-level string
interpretCmd :: forall command r a.
                ( KnownSymbol command
                , Member Cmds r
                )
             => Sem (Cmd command : r) a
             -> Sem r a
interpretCmd = interpret $ \case
    CmdExecWithStdin cwd args stdinContent ->
        let commandName = symbolVal (Proxy @command)
        in send (CmdsExecWithStdin cwd commandName args stdinContent)
