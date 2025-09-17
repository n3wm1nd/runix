{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Runix.Runners.CLI.Single (singleTaskRunner) where

import Prelude hiding (readFile, writeFile, error)
import System.Exit (exitFailure)
import System.IO (stderr)
import GHC.IO.Handle (hPutStr)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL

import Polysemy
import Runix.Runner (runUntrusted, SafeEffects)

-- | Simple CLI runner for a single task
-- Takes task function directly, reads JSON from stdin, writes JSON to stdout
singleTaskRunner :: (FromJSON input, ToJSON output)
                 => (input -> (forall r . Members SafeEffects r => Sem r output))
                 -> IO ()
singleTaskRunner taskFunc = do
  result <- do
    input <- BL.getContents >>= either (const $ fail "Failed to parse input JSON") return . eitherDecode
    runUntrusted (taskFunc input)

  case result of
    Right output -> BL.putStr (encode output)
    Left errMsg -> hPutStr stderr errMsg >> exitFailure