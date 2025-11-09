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
import qualified Data.Text as T
import Data.String (fromString)
import Data.Maybe (mapMaybe)
import Control.Monad (filterM)
import Polysemy
import GHC.Stack
import Runix.Cmd.Effects (Cmd, cmdExec)
import qualified Runix.Cmd.Effects as CmdE
import Runix.Logging.Effects (Logging, info)
import Runix.FileSystem.Effects (FileSystemRead, fileExists)

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

-- | Grep interpreter using ripgrep
grepIO :: HasCallStack => Members [Cmd, Logging, FileSystemRead] r => Sem (Grep : r) a -> Sem r a
grepIO = interpret $ \case
    GrepSearch basePath pattern -> do
        info $ fromString "grep search: " <> fromString pattern <> fromString " in " <> fromString basePath
        result <- cmdExec "rg" ["--line-number", "--with-filename", "--", pattern, basePath]
        let allMatches = case CmdE.exitCode result of
                0 -> parseRipgrepOutput $ lines $ T.unpack $ CmdE.stdout result
                _ -> []
        -- Filter results to only include files that pass FileSystem access controls
        filterM (\match -> fileExists (matchFile match)) allMatches
  where
    parseRipgrepOutput :: [String] -> [GrepMatch]
    parseRipgrepOutput = mapMaybe parseLine
      where
        parseLine :: String -> Maybe GrepMatch
        parseLine line = case break (== ':') line of
            (file, ':' : rest) -> case break (== ':') rest of
                (lineNumStr, ':' : text) -> case reads lineNumStr of
                    [(lineNum, "")] -> Just $ GrepMatch
                        { matchFile = file
                        , matchLine = lineNum
                        , matchText = T.pack text
                        }
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
