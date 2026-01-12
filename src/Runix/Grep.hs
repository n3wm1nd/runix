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

module Runix.Grep where

import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (fromString)
import Data.Maybe (mapMaybe)
import Control.Monad (filterM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Polysemy
import Polysemy.Fail (Fail)
import GHC.Stack
import Runix.Cmd (Cmd, cmdExec)
import qualified Runix.Cmd as CmdE
import Runix.Logging (Logging, info)
import Runix.FileSystem.System (FileSystemRead, fileExists)

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

-- TODO: this is using the filesystem.system effects, it should work with any filesystem that translates to system paths
-- | Grep interpreter using ripgrep
grepIO :: HasCallStack => Members [Cmd, Logging, FileSystemRead, Fail] r => Sem (Grep : r) a -> Sem r a
grepIO = interpret $ \case
    GrepSearch basePath pattern -> do
        info $ fromString "grep search: " <> fromString pattern <> fromString " in " <> fromString basePath
        result <- cmdExec "rg" ["--line-number", "--with-filename", "--", pattern, basePath]
        let allMatches = case CmdE.exitCode result of
                0 -> parseRipgrepOutput $ lines $ T.unpack $ CmdE.stdout result
                _ -> []
        -- Filter results to only include files that pass FileSystem access controls
        -- Group by file first to check each file only once
        let matchesByFile = Map.fromListWith (++) [(matchFile m, [m]) | m <- allMatches]
        allowedFiles <- filterM fileExists (Map.keys matchesByFile)
        let allowedSet = Map.fromList [(f, ()) | f <- allowedFiles]
        return $ filter (\m -> Map.member (matchFile m) allowedSet) allMatches
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
