{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Runix.Runners.CLI.Multi (Task(..), multiTaskRunner) where

import Prelude hiding (readFile, writeFile, error)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)
import GHC.IO.Handle (hPutStr)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (find, uncons)

import Runix.Runner (runUntrusted, SafeEffects)
import Runix.LLM.OpenRouter (OpenRouter, OpenRouterModel)
import Polysemy
import Polysemy.Error

-- | Task representation for multi-task CLI
data Task where
  Task :: (FromJSON a, ToJSON b) =>
    { taskName :: String
    , taskFunc :: a -> (forall r . Members (SafeEffects OpenRouter OpenRouterModel) r => Sem r b)
    } -> Task

-- | CLI Runner for multiple tasks - user selects task by command line argument
multiTaskRunner :: [Task] -> IO ()
multiTaskRunner tasks = do
  result <- runM . runError $ do
    args <- embed getArgs
    tName <- fromEither $ maybe (Left "no taskname given") Right $ fmap fst (uncons args)

    -- Find the task by name
    Task _name taskFn <-
      fromEither $ maybe (Left ("Error: Task '" <> tName <> "' not found")) Right $
      find ( (== tName) . taskName) tasks

    input <- embed BL.getContents >>= fromEither . eitherDecode

    output <- embed $ runUntrusted (taskFn input)
    fromEither $ fmap encode output
  case result of
    Right o -> BL.putStr o
    Left e -> hPutStr stderr e >> exitFailure