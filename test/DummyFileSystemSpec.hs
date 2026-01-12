{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module DummyFileSystemSpec
  ( spec
  , InMemoryFS
  , filesystemReadInMemory
  , runFS
  , runFSWithFail
  ) where

import Test.Hspec
import Polysemy
import Polysemy.Error
import Polysemy.Fail (Fail, failToError)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Prelude hiding (readFile)
import Data.List (isInfixOf)

import Runix.FileSystem.System
import Runix.FileSystem.InMemory

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Run a filesystem program with the in-memory interpreter
runFS :: FilePath -> InMemoryFS -> Sem '[FileSystemRead, Error String] a -> Either String a
runFS cwd fs = run . runError @String . filesystemReadInMemory cwd fs

-- | Run a filesystem program that may fail
runFSWithFail :: FilePath -> InMemoryFS -> Sem [FileSystemRead, Fail, Error String] a -> Either String a
runFSWithFail cwd fs prog =
  run
    . runError @String
    . failToError id
    . filesystemReadInMemory cwd fs
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
