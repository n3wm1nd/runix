{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GrepSecuritySpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Error
import Polysemy.Fail (Fail, failToError)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.List (isInfixOf, sort)
import Data.String (fromString)
import System.FilePath ((</>), normalise)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing)
import Prelude hiding (readFile, writeFile)

import Runix.Grep
import Runix.FileSystem
import qualified Runix.FileSystem.System as System
import Runix.Cmd (Cmds, cmdsIO)
import Runix.Logging (Logging, loggingIO)

--------------------------------------------------------------------------------
-- Test Project Type
--------------------------------------------------------------------------------

-- | Test project representing a chrooted filesystem
newtype GrepTest = GrepTest FilePath deriving (Show, Eq)

instance HasProjectPath GrepTest where
  getProjectPath (GrepTest p) = p

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

-- | Run grep test with actual filesystem in temp directory
-- Sets up temp directory structure and runs test action
withTempGrepTest :: (FilePath -> IO ()) -> IO ()
withTempGrepTest action = withSystemTempDirectory "grep-test" $ \tmpDir -> do
  -- Create directory structure:
  -- tmpDir/
  --   outside.txt           <- "outside content" (should not be accessible)
  --   secrets.txt           <- "secret data" (should not be accessible)
  --   project/              <- chroot root
  --     allowed.txt         <- "allowed content"
  --     test.txt            <- "test content"
  --     subdir/
  --       file.txt          <- "subdir content"
  --       nested.txt        <- "nested content"

  let projectDir = tmpDir </> "project"
      subdirPath = projectDir </> "subdir"

  createDirectoryIfMissing True subdirPath

  -- Files outside chroot
  BS.writeFile (tmpDir </> "outside.txt") "outside content"
  BS.writeFile (tmpDir </> "secrets.txt") "secret data"

  -- Files inside chroot
  BS.writeFile (projectDir </> "allowed.txt") "allowed content"
  BS.writeFile (projectDir </> "test.txt") "test content"
  BS.writeFile (subdirPath </> "file.txt") "subdir content"
  BS.writeFile (subdirPath </> "nested.txt") "nested content"

  action projectDir

-- | Run a grep operation with security filter
-- The filter is applied AFTER chroot, so it sees chroot-relative paths
runGrepWithSecurity :: FilePath  -- ^ Project root (system path for chroot)
                    -> FilePath  -- ^ Allowed subpath (chroot-relative, e.g. "/" or "/subdir")
                    -> FilePath  -- ^ Search base path (chroot-relative)
                    -> String    -- ^ Search pattern
                    -> IO (Either String [GrepMatch])
runGrepWithSecurity projectRoot allowedPath basePath pattern = do
  let proj = GrepTest projectRoot
  runM
    . runError @String
    . failToError id
    . loggingIO
    . cmdsIO
    . System.filesystemIO
    . fileSystemLocal proj
    . filterFileSystem @GrepTest (limitToSubpath allowedPath)
    . grepForFilesystem @GrepTest
    $ grepSearch @GrepTest basePath pattern

-- | Run grep without security filter (for comparison)
runGrepNoSecurity :: FilePath  -- ^ Project root (chroot)
                  -> FilePath  -- ^ Search base path
                  -> String    -- ^ Search pattern
                  -> IO (Either String [GrepMatch])
runGrepNoSecurity projectRoot basePath pattern = do
  let proj = GrepTest projectRoot
  runM
    . runError @String
    . failToError id
    . loggingIO
    . cmdsIO
    . System.filesystemIO
    . fileSystemLocal proj
    . grepForFilesystem @GrepTest
    $ grepSearch @GrepTest basePath pattern

-- | Run grep using System interpreter (non-chrooted)
runGrepSystem :: FilePath  -- ^ Search base path (system path)
              -> String    -- ^ Search pattern
              -> IO (Either String [GrepMatch])
runGrepSystem basePath pattern =
  runM
    . runError @String
    . failToError id
    . loggingIO
    . cmdsIO
    . System.filesystemReadIO
    . grepIO
    $ grepSearch @() basePath pattern

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Basic Grep Functionality" $ do
    it "finds content in files within project" $ do
      withTempGrepTest $ \projectDir -> do
        result <- runGrepNoSecurity projectDir "/" "allowed"
        case result of
          Right matches -> do
            length matches `shouldBe` 1
            matchFile (head matches) `shouldBe` "/allowed.txt"
            T.isInfixOf "allowed" (matchText (head matches)) `shouldBe` True
          Left err -> expectationFailure $ "Grep failed: " ++ err

    it "finds content in subdirectories" $ do
      withTempGrepTest $ \projectDir -> do
        result <- runGrepNoSecurity projectDir "/" "subdir"
        case result of
          Right matches -> do
            length matches `shouldBe` 1
            matchFile (head matches) `shouldBe` "/subdir/file.txt"
          Left err -> expectationFailure $ "Grep failed: " ++ err

    it "returns multiple matches from different files" $ do
      withTempGrepTest $ \projectDir -> do
        result <- runGrepNoSecurity projectDir "/" "content"
        case result of
          Right matches -> do
            -- Should match: allowed.txt, test.txt, file.txt, nested.txt
            length matches `shouldBe` 4
            let files = sort $ map matchFile matches
            files `shouldBe` ["/allowed.txt", "/subdir/file.txt", "/subdir/nested.txt", "/test.txt"]
          Left err -> expectationFailure $ "Grep failed: " ++ err

    it "searches in specific subdirectory" $ do
      withTempGrepTest $ \projectDir -> do
        result <- runGrepNoSecurity projectDir "/subdir" "content"
        case result of
          Right matches -> do
            length matches `shouldBe` 2
            all (\m -> "/subdir" `isInfixOf` matchFile m) matches `shouldBe` True
          Left err -> expectationFailure $ "Grep failed: " ++ err

    it "returns empty list when pattern not found" $ do
      withTempGrepTest $ \projectDir -> do
        result <- runGrepNoSecurity projectDir "/" "nonexistent"
        case result of
          Right matches -> matches `shouldBe` []
          Left err -> expectationFailure $ "Grep failed: " ++ err

  describe "Chroot Security - Path Traversal Prevention" $ do
    it "chroot prevents ripgrep from escaping via ../ in base path" $ do
      -- Chroot prevents escape attempts via ../ - verify it returns empty results
      withTempGrepTest $ \projectDir -> do
        -- Try to search outside chroot using /../ (should resolve to / inside chroot)
        result <- runGrepNoSecurity projectDir "/../" "outside"
        -- Should succeed but find nothing (outside.txt is outside chroot)
        result `shouldBe` Right []

    it "ripgrep can search subdirectories within chroot" $ do
      -- Verify that normal subdirectory search works (baseline functionality)
      withTempGrepTest $ \projectDir -> do
        result <- runGrepNoSecurity projectDir "/subdir" "content"
        case result of
          Right matches -> length matches `shouldBe` 2
          Left err -> expectationFailure $ "Grep failed: " ++ err

  describe "Security Filter - limitToSubpath (filter AFTER chroot)" $ do
    it "allows grep within entire chroot when filter allows /" $ do
      withTempGrepTest $ \projectDir -> do
        -- Filter applied after chroot: allow "/" means whole chroot
        result <- runGrepWithSecurity projectDir "/" "/" "allowed"
        case result of
          Right matches -> do
            length matches `shouldBe` 1
            matchFile (head matches) `shouldBe` "/allowed.txt"
          Left err -> expectationFailure $ "Grep failed: " ++ err

    it "allows grep in subdirectory when filter allows whole chroot" $ do
      withTempGrepTest $ \projectDir -> do
        -- Filter allows "/", search in "/subdir"
        result <- runGrepWithSecurity projectDir "/" "/subdir" "subdir"
        case result of
          Right matches -> do
            length matches `shouldBe` 1
            matchFile (head matches) `shouldBe` "/subdir/file.txt"
          Left err -> expectationFailure $ "Grep failed: " ++ err

    it "blocks grep with base path outside allowed subdirectory" $ do
      withTempGrepTest $ \projectDir -> do
        -- Filter restricts to "/subdir" (chroot-relative), try to search from "/"
        result <- runGrepWithSecurity projectDir "/subdir" "/" "content"
        case result of
          Left err -> "Access denied" `shouldSatisfy` (`isInfixOf` err)
          Right _ -> expectationFailure "Should have blocked access to parent directory"

    it "filters grep results from files outside allowed subdirectory" $ do
      withTempGrepTest $ \projectDir -> do
        -- Filter restricts to "/subdir", search within "/subdir"
        -- Even if grep somehow returns files from /, they should be filtered
        result <- runGrepWithSecurity projectDir "/subdir" "/subdir" "content"
        case result of
          Right matches -> do
            -- All results must be in /subdir
            all (\m -> "/subdir" `isInfixOf` matchFile m) matches `shouldBe` True
            length matches `shouldBe` 2  -- file.txt and nested.txt
          Left err -> expectationFailure $ "Grep failed: " ++ err

    it "handles relative base paths correctly with filter" $ do
      withTempGrepTest $ \projectDir -> do
        -- Filter allows "/", CWD is /, relative path "subdir"
        -- Input is relative, so output should be relative too
        result <- runGrepWithSecurity projectDir "/" "subdir" "nested"
        case result of
          Right matches -> do
            length matches `shouldBe` 1
            matchFile (head matches) `shouldBe` "subdir/nested.txt"
          Left err -> expectationFailure $ "Grep failed: " ++ err

  describe "System Grep (Non-Chrooted)" $ do
    it "searches using actual system paths" $ do
      withTempGrepTest $ \projectDir -> do
        -- Use System grep with actual filesystem path
        result <- runGrepSystem projectDir "allowed"
        case result of
          Right matches -> do
            length matches `shouldBe` 1
            -- System grep returns system paths
            matchFile (head matches) `shouldBe` (projectDir </> "allowed.txt")
          Left err -> expectationFailure $ "Grep failed: " ++ err

    it "searches in subdirectory using system paths" $ do
      withTempGrepTest $ \projectDir -> do
        let subdirPath = projectDir </> "subdir"
        result <- runGrepSystem subdirPath "subdir"
        case result of
          Right matches -> do
            length matches `shouldBe` 1
            matchFile (head matches) `shouldBe` (subdirPath </> "file.txt")
          Left err -> expectationFailure $ "Grep failed: " ++ err

  describe "Pattern Security" $ do
    it "grep pattern does not escape directory boundaries" $ do
      -- The pattern itself is just a regex, not a path
      -- But let's verify no weird escapes happen
      withTempGrepTest $ \projectDir -> do
        -- Use a pattern that might look like a path
        result <- runGrepNoSecurity projectDir "/" "\\.\\./outside"
        case result of
          Right matches -> matches `shouldBe` []  -- Won't match anything
          Left err -> expectationFailure $ "Grep failed: " ++ err

  describe "Filter Composition with hideGit (filter AFTER chroot)" $ do
    it "combines limitToSubpath with hideGit filter" $ do
      withTempGrepTest $ \projectDir -> do
        -- Add a .git directory with content
        let gitDir = projectDir </> ".git"
        createDirectoryIfMissing True gitDir
        BS.writeFile (gitDir </> "config") "git config content"

        -- Filter applied AFTER chroot: use chroot-relative path "/"
        let combinedFilter = limitToSubpath "/" <> hideGit
            proj = GrepTest projectDir

        result <- runM
          . runError @String
          . failToError id
          . loggingIO
          . cmdsIO
          . System.filesystemIO
          . fileSystemLocal proj
          . filterFileSystem @GrepTest combinedFilter
          . grepForFilesystem @GrepTest
          $ grepSearch @GrepTest "/" "content"

        case result of
          Right matches -> do
            -- Should not include .git files
            all (\m -> not $ "/.git/" `isInfixOf` matchFile m) matches `shouldBe` True
            -- Should still find other files (4 files with "content")
            length matches `shouldBe` 4
          Left err -> expectationFailure $ "Grep failed: " ++ err

  describe "Edge Cases" $ do
    it "handles empty directory" $ do
      withSystemTempDirectory "grep-empty" $ \tmpDir -> do
        let projectDir = tmpDir </> "empty"
        createDirectoryIfMissing True projectDir
        result <- runGrepNoSecurity projectDir "/" "anything"
        case result of
          Right matches -> matches `shouldBe` []
          Left _ -> return ()  -- Also acceptable

    it "handles file with special characters in path" $ do
      withSystemTempDirectory "grep-special" $ \tmpDir -> do
        let projectDir = tmpDir </> "project"
            specialDir = projectDir </> "dir with spaces"
        createDirectoryIfMissing True specialDir
        BS.writeFile (specialDir </> "file.txt") "special content"

        result <- runGrepNoSecurity projectDir "/" "special"
        case result of
          Right matches -> do
            length matches `shouldBe` 1
            matchFile (head matches) `shouldContain` "dir with spaces"
          Left err -> expectationFailure $ "Grep failed: " ++ err

    it "handles large number of matches" $ do
      withSystemTempDirectory "grep-many" $ \tmpDir -> do
        let projectDir = tmpDir </> "project"
        createDirectoryIfMissing True projectDir
        -- Create many files with same content
        mapM_ (\i -> BS.writeFile (projectDir </> ("file" ++ show i ++ ".txt")) "common") [1..50 :: Int]

        result <- runGrepNoSecurity projectDir "/" "common"
        case result of
          Right matches -> length matches `shouldBe` 50
          Left err -> expectationFailure $ "Grep failed: " ++ err
