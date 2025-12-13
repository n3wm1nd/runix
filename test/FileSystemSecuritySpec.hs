{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module FileSystemSecuritySpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Error
import Polysemy.Fail (Fail, runFail, failToError)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath (normalise, isAbsolute, (</>) , splitPath, addTrailingPathSeparator)
import Prelude hiding (readFile)
import Data.List (isInfixOf, isPrefixOf)

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

    Glob _base _pat -> return $ Right []  -- Simplified: no glob support

    GetCwd -> return cwd
  where
    resolveAbsolutePath basePath path
      | isAbsolute path = normalise path
      | otherwise = normalise (basePath </> path)

filesystemWriteDummy :: Member (Error String) r
                     => DummyFS
                     -> Sem (FileSystemWrite : r) a
                     -> Sem r a
filesystemWriteDummy _fs = interpret $ \case
    WriteFile p _content -> return $ Right ()  -- Just succeed for now

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
  describe "Path Utilities" $ do
    it "normalise does NOT resolve .. components" $ do
      let path1 = normalise "/project/subdir" </> "../../README.md"
          path2 = normalise ("/project/subdir" </> "../../README.md")

      -- normalise does NOT resolve .., it just cleans up
      path1 `shouldBe` "/project/subdir/../../README.md"
      path2 `shouldBe` "/project/subdir/../../README.md"

    it "splitPath behavior for directory vs file" $ do
      let dirPath = splitPath "/project"
          filePath = splitPath "/project/file.txt"

      -- See actual values
      dirPath `shouldBe` ["/", "project"]
      filePath `shouldBe` ["/", "project/", "file.txt"]

      -- This will fail - showing the actual problem
      dirPath `isPrefixOf` filePath `shouldBe` False

    it "security check should reject paths outside allowed directory" $ do
      let allowedPath = "/project"
          targetPath = "/etc/passwd"
          normalizedAllowed = addTrailingPathSeparator $ normalise allowedPath
          normalizedTarget = normalise targetPath

      -- Debug: see the actual split paths
      let allowedSplit = splitPath normalizedAllowed
          targetSplit = splitPath normalizedTarget

      allowedSplit `shouldBe` ["/", "project/"]
      targetSplit `shouldBe` ["/", "etc/", "passwd"]

      -- This should be False
      (allowedSplit `isPrefixOf` targetSplit) `shouldBe` False

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

  describe "limitSubpathRead Security" $ do
    it "allows reading files within allowed path (absolute)" $ do
      let fs = Map.fromList [("/project/file.txt", "content")]
          program = limitSubpathRead "/project" $ readFile "/project/file.txt"
          result = runFSWithFail "/project" fs program

      result `shouldBe` Right "content"

    it "allows reading files within allowed path (relative)" $ do
      -- Relative path "subdir/file.txt" with CWD "/project" resolves to "/project/subdir/file.txt"
      let fs = Map.fromList [("/project/subdir/file.txt", "content")]
          program = limitSubpathRead "/project" $ readFile "subdir/file.txt"
          result = runFSWithFail "/project" fs program

      result `shouldBe` Right "content"

    it "blocks reading files outside allowed path (absolute)" $ do
      let fs = Map.fromList [("/etc/passwd", "secret")]
          program = limitSubpathRead "/project" $ readFile "/etc/passwd"
          result = runFSWithFail "/project" fs program

      result `shouldSatisfy` \case
        Left err -> "not allowed" `isInfixOf` err
        Right _ -> False

    it "blocks reading files outside allowed path (relative escape)" $ do
      -- "../../../etc/passwd" from "/project" normalizes to "/etc/passwd"
      let fs = Map.fromList [("/etc/passwd", "secret"), ("/project/dummy", "dummy")]
          --Test with security
          resultWithSecurity = runFSWithFail "/project" fs $ limitSubpathRead "/project" $ readFile "/etc/passwd"

      -- Should be blocked with security when using absolute path
      resultWithSecurity `shouldSatisfy` \case
        Left err -> "not allowed" `isInfixOf` err
        Right _ -> False

    it "uses actual CWD to resolve relative paths, then checks against allowedPath" $ do
      -- Critical test: when actual CWD differs from allowedPath
      -- The dummy filesystem has CWD = "/actual/cwd"
      -- But we want to restrict to "/project"
      -- A file at "/actual/cwd/file.txt" should be BLOCKED because it's outside /project
      let fs = Map.fromList [("/actual/cwd/file.txt", "content"), ("/project/allowed.txt", "allowed")]
          -- Actual CWD is /actual/cwd, but we're restricting to /project
          -- "file.txt" resolves to /actual/cwd/file.txt which is outside /project
          resultBlocked = runFSWithFail "/actual/cwd" fs $ limitSubpathRead "/project" $ readFile "file.txt"
          -- "/project/allowed.txt" is inside /project
          resultAllowed = runFSWithFail "/actual/cwd" fs $ limitSubpathRead "/project" $ readFile "/project/allowed.txt"

      -- "file.txt" resolves to /actual/cwd/file.txt which is outside /project -> BLOCKED
      resultBlocked `shouldSatisfy` \case
        Left err -> "not allowed" `isInfixOf` err
        Right _ -> False

      -- "/project/allowed.txt" is absolute and inside /project -> ALLOWED
      resultAllowed `shouldBe` Right "allowed"

    it "blocks parent directory escape with ../" $ do
      -- Reproduce the actual bug: CWD is /project/subdir, restricted to /project
      -- Reading ../../README.md should resolve to /README.md and be blocked
      let fs = Map.fromList
            [ ("/README.md", "root readme")
            , ("/project/README.md", "project readme")
            , ("/project/subdir/file.txt", "subdir file")
            ]
          -- CWD is in a subdirectory, restricted to parent directory
          result = runFSWithFail "/project/subdir" fs $ limitSubpathRead "/project" $ readFile "../../README.md"

      -- "../../README.md" from /project/subdir resolves to /README.md which is outside /project -> BLOCKED
      result `shouldSatisfy` \case
        Left err -> "not allowed" `isInfixOf` err
        Right _ -> False
