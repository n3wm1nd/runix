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
import Runix.Cmd (Cmds, cmdsExec)
import qualified Runix.Cmd as CmdE
import Runix.Logging (Logging, info, loggingNull)
import Runix.FileSystem.System (FileSystemRead, fileExists)
import qualified Runix.FileSystem as FS
import Runix.FileSystem (HasProjectPath(..))
import System.FilePath (isAbsolute)

-- | Grep search result
data GrepMatch = GrepMatch
  { matchFile :: FilePath
  , matchLine :: Int
  , matchText :: Text
  } deriving (Show, Eq)

-- | Grep effect for searching file contents
-- Parameterized by project/filesystem type to work with chrooted filesystems
data Grep project (m :: Type -> Type) a where
    -- Search for pattern in files under basePath
    GrepSearch :: FilePath -> String -> Grep project m [GrepMatch]

makeSem ''Grep

-- | Type alias for non-parameterized system-level grep
-- Used when working with System.FileSystemRead (not chrooted)
type GrepSystem = Grep ()

-- | Grep interpreter using ripgrep (for System filesystem) with verbose logging
-- Works with non-parameterized System.FileSystemRead
grepIOVerbose :: HasCallStack => Members [Cmds, FileSystemRead, Logging, Fail] r => Sem (GrepSystem : r) a -> Sem r a
grepIOVerbose = interpret $ \case
    GrepSearch basePath pattern -> do
        info $ fromString "grep search: " <> fromString pattern <> fromString " in " <> fromString basePath
        result <- cmdsExec "rg" ["--line-number", "--with-filename", "--", pattern, basePath]
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

-- | Grep interpreter using ripgrep (quiet version)
-- Runs the verbose version but discards logging
grepIO :: HasCallStack => Members [Cmds, FileSystemRead, Fail] r => Sem (GrepSystem : r) a -> Sem r a
grepIO = loggingNull . grepIOVerbose . raiseUnder

-- | Grep interpreter that works with parameterized chrooted filesystems with verbose logging
-- Translates paths from chroot coordinates to system coordinates for ripgrep,
-- then translates results back to chroot coordinates
grepForFilesystemVerbose :: forall project r a.
                            ( HasCallStack
                            , HasProjectPath project
                            , Members '[FS.FileSystem project, FS.FileSystemRead project, Cmds, Logging, Fail] r
                            )
                         => Sem (Grep project : r) a
                         -> Sem r a
grepForFilesystemVerbose = interpret $ \case
    GrepSearch basePath pattern -> do
        -- Get project configuration and virtual CWD
        proj <- FS.getFileSystem @project
        virtualCwd <- FS.getCwd @project

        -- Translate base path to system path for ripgrep
        let systemBasePath = FS.translateToSystemPath proj virtualCwd basePath
            inputWasAbsolute = isAbsolute basePath

        info $ fromString "grep search: " <> fromString pattern <> fromString " in " <> fromString basePath
             <> fromString " (system path: " <> fromString systemBasePath <> fromString ")"

        -- Check if base path is accessible (triggers filter check)
        -- If the filter blocks access, this will fail
        _ <- FS.isDirectory @project basePath

        -- Run ripgrep with system path
        result <- cmdsExec "rg" ["--line-number", "--with-filename", "--", pattern, systemBasePath]
        let allMatches = case CmdE.exitCode result of
                0 -> parseRipgrepOutput $ lines $ T.unpack $ CmdE.stdout result
                _ -> []

        -- Translate all file paths in results back to chroot coordinates (preserving relative/absolute)
        let translatePath = FS.translateFromSystemPath' proj virtualCwd inputWasAbsolute
            translatedMatches = map (\m -> m { matchFile = translatePath (matchFile m) }) allMatches

        -- Filter results to only include files that pass FileSystem access controls
        let matchesByFile = Map.fromListWith (++) [(matchFile m, [m]) | m <- translatedMatches]
        allowedFiles <- filterM (FS.fileExists @project) (Map.keys matchesByFile)
        let allowedSet = Map.fromList [(f, ()) | f <- allowedFiles]
        return $ filter (\m -> Map.member (matchFile m) allowedSet) translatedMatches
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

-- | Grep interpreter for parameterized filesystems (quiet version)
-- Runs the verbose version but discards logging
grepForFilesystem :: forall project r a.
                     ( HasCallStack
                     , HasProjectPath project
                     , Members '[FS.FileSystem project, FS.FileSystemRead project, Cmds, Fail] r
                     )
                  => Sem (Grep project : r) a
                  -> Sem r a
grepForFilesystem = loggingNull . grepForFilesystemVerbose . raiseUnder
