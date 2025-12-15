{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module DummyFileSystemSpec
  ( spec
  , DummyFS
  , filesystemReadDummy
  , runFS
  , runFSWithFail
  ) where

import Test.Hspec
import Polysemy
import Polysemy.Error
import Polysemy.Fail (Fail, runFail, failToError)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath (normalise, isAbsolute, (</>) , splitPath, addTrailingPathSeparator, takeFileName, takeDirectory, splitDirectories, joinPath)
import Prelude hiding (readFile)
import Data.List (isInfixOf, isPrefixOf, sort)
import qualified System.FilePath.Glob as Glob

import Runix.FileSystem.Effects

--------------------------------------------------------------------------------
-- Dummy Filesystem Interpreter
--------------------------------------------------------------------------------

-- | In-memory filesystem for testing
type DummyFS = Map FilePath BS.ByteString

-- | Dummy interpreter that uses an in-memory filesystem
-- Takes a current working directory and a map of files
filesystemReadDummy :: Member (Error String) r
                    => FilePath  -- ^ Current working directory
                    -> DummyFS   -- ^ In-memory filesystem
                    -> Sem (FileSystemRead : r) a
                    -> Sem r a
filesystemReadDummy cwd fs = interpret $ \case
    ReadFile p -> return $ case Map.lookup (resolveAbsolutePath cwd p) fs of
        Just content -> Right content
        Nothing -> Left $ "File not found: " ++ p

    ListFiles p -> return $ case Map.lookup (resolveAbsolutePath cwd p) fs of
        Just _ -> Left $ "Not a directory: " ++ p
        Nothing -> Left $ "Directory not found: " ++ p

    FileExists p -> return $ Right $ Map.member (resolveAbsolutePath cwd p) fs

    IsDirectory _p -> return $ Right False  -- Simplified: no directories in our dummy FS

    Glob base pat -> do
        -- Glob works like this:
        -- 1. Resolve base path (absolute or relative to cwd)
        -- 2. Pattern can be absolute or relative
        -- 3. If pattern is absolute, use it directly
        -- 4. If pattern is relative, it's relative to base
        -- 5. Return paths as they appear relative to base
        let basePath = resolveAbsolutePath cwd base

            -- Determine the search directory:
            -- If pattern starts with /, it's absolute
            -- Otherwise, resolve pattern relative to base
            (searchDir, searchPattern) =
              if isAbsolute pat
              then
                -- Absolute pattern: extract directory and pattern parts
                let dir = takeDirectory pat
                    patName = takeFileName pat
                in (dir, patName)
              else
                -- Relative pattern: navigate from base according to pattern
                -- Pattern can contain .. to go up from base
                let patDir = takeDirectory pat
                    patName = takeFileName pat
                    resolvedDir = basicResolvePath (basePath </> patDir)
                in (resolvedDir, patName)

            -- Get all files in filesystem
            allFiles = Map.keys fs

            -- Compile the final search pattern (just the filename part)
            finalPattern = Glob.compile searchPattern

            -- Find files under searchDir that match the pattern name
            matchedAbsolute = filter (\f ->
              let dir = takeDirectory f
                  name = takeFileName f
              in dir == searchDir && Glob.match finalPattern name) allFiles

            -- Convert to paths relative to base
            matchedRelative = map (makeRelativeTo basePath) matchedAbsolute

        return $ Right $ sort matchedRelative
      where
        makeRelativeTo :: FilePath -> FilePath -> FilePath
        makeRelativeTo base target =
          let baseParts = splitDirectories (normalise base)
              targetParts = splitDirectories (normalise target)
              common = length $ takeWhile id $ zipWith (==) baseParts targetParts
              ups = replicate (length baseParts - common) ".."
              downs = drop common targetParts
              rel = ups ++ downs
          in if null rel then "." else joinPath rel

    GetCwd -> return cwd
  where
    resolveAbsolutePath basePath path
      | isAbsolute path = basicResolvePath path
      | otherwise = basicResolvePath (basePath </> path)

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Run a filesystem program with the dummy interpreter
runFS :: FilePath -> DummyFS -> Sem '[FileSystemRead, Error String] a -> Either String a
runFS cwd fs = run . runError @String . filesystemReadDummy cwd fs

-- | Run a filesystem program that may fail
runFSWithFail :: FilePath -> DummyFS -> Sem [FileSystemRead, Fail, Error String] a -> Either String a
runFSWithFail cwd fs prog =
  run
    . runError @String
    . failToError id
    . filesystemReadDummy cwd fs
    $ prog

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Dummy Filesystem Interpreter" $ do
    it "returns file content for existing files" $ do
      let fs = Map.fromList [("/test/file.txt", "Hello, World!")]
          result = runFSWithFail "/test" fs $ readFile "/test/file.txt"

      result `shouldBe` Right "Hello, World!"

    it "returns error for non-existing files" $ do
      let fs = Map.fromList [("/test/file.txt", "Hello, World!")]
          result = runFSWithFail "/test" fs $ readFile "/test/missing.txt"

      result `shouldSatisfy` \case
        Left err -> "File not found" `isInfixOf` err
        Right _ -> False

    it "fileExists returns True for existing files" $ do
      let fs = Map.fromList [("/test/file.txt", "content")]
          result = runFSWithFail "/test" fs $ fileExists "/test/file.txt"

      result `shouldBe` Right True

    it "fileExists returns False for missing files" $ do
      let fs = Map.fromList []
          result = runFSWithFail "/test" fs $ fileExists "/test/missing.txt"

      result `shouldBe` Right False

    it "getCwd returns the working directory" $ do
      let fs = Map.empty
          result = runFS "/home/user/project" fs getCwd

      result `shouldBe` Right "/home/user/project"

  describe "Glob Operations" $ do
    it "glob returns matching files within base directory" $ do
      let fs = Map.fromList
            [ ("/project/file1.txt", "content1")
            , ("/project/file2.txt", "content2")
            , ("/project/doc.md", "markdown")
            , ("/other/file3.txt", "content3")
            ]
          result = runFSWithFail "/project" fs $ glob "/project" "*.txt"

      -- Glob returns paths relative to base
      result `shouldBe` Right ["file1.txt", "file2.txt"]

    it "glob works with relative base path" $ do
      let fs = Map.fromList
            [ ("/project/subdir/file1.txt", "content1")
            , ("/project/subdir/file2.txt", "content2")
            , ("/project/subdir/doc.md", "markdown")
            ]
          -- "subdir" relative to CWD "/project" -> "/project/subdir"
          result = runFSWithFail "/project" fs $ glob "subdir" "*.txt"

      result `shouldBe` Right ["file1.txt", "file2.txt"]

    it "glob matches patterns with wildcards" $ do
      let fs = Map.fromList
            [ ("/project/test_file.txt", "content1")
            , ("/project/test_data.txt", "content2")
            , ("/project/prod_file.txt", "content3")
            ]
          result = runFSWithFail "/project" fs $ glob "/project" "test_*.txt"

      result `shouldBe` Right ["test_data.txt", "test_file.txt"]

    it "glob returns empty list when no matches" $ do
      let fs = Map.fromList
            [ ("/project/file1.txt", "content1")
            , ("/project/file2.txt", "content2")
            ]
          result = runFSWithFail "/project" fs $ glob "/project" "*.md"

      result `shouldBe` Right []

    it "glob with pattern containing .. navigates up from base" $ do
      let fs = Map.fromList
            [ ("/project/file.txt", "project file")
            , ("/project/subdir/nested.txt", "nested")
            ]
          -- Base is /project/subdir, pattern is ../*.txt
          -- Should search in /project for *.txt
          result = runFSWithFail "/project/subdir" fs $ glob "." "../*.txt"

      -- Result should be ../file.txt (relative to base /project/subdir)
      result `shouldBe` Right ["../file.txt"]

    it "glob with pattern containing ../../ escapes outside base" $ do
      let fs = Map.fromList
            [ ("/project/subdir/file.txt", "subdir file")
            , ("/file.txt", "root file")
            ]
          -- Base is /project/subdir (from cwd and ".")
          -- Pattern ../../*.txt should search in / for *.txt
          result = runFSWithFail "/project/subdir" fs $ glob "." "../../*.txt"

      -- Should find /file.txt, relative path from base is ../../file.txt
      result `shouldBe` Right ["../../file.txt"]
