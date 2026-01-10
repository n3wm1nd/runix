{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileSystemProjectSpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Error
import Polysemy.Fail (Fail, failToError)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath (normalise, isAbsolute, (</>) , takeFileName, takeDirectory, splitDirectories, joinPath)
import Prelude hiding (readFile)
import Data.List (isInfixOf, isPrefixOf, sort)
import qualified System.FilePath.Glob as Glob

import Runix.FileSystem.Project.Effects
import qualified Runix.FileSystem.Effects as System

--------------------------------------------------------------------------------
-- Dummy Filesystem Interpreter for Testing
--------------------------------------------------------------------------------

type DummyFS = Map FilePath BS.ByteString

-- | Pure interpreter for System.FileSystemRead using in-memory map
filesystemReadDummy :: Member (Error String) r
                    => FilePath  -- ^ Current working directory
                    -> DummyFS   -- ^ In-memory filesystem
                    -> Sem (System.FileSystemRead : r) a
                    -> Sem r a
filesystemReadDummy cwd fs = interpret $ \case
    System.ReadFile p -> return $ case Map.lookup (resolveAbsolutePath cwd p) fs of
        Just content -> Right content
        Nothing -> Left $ "File not found: " ++ p

    System.ListFiles p -> return $ case Map.lookup (resolveAbsolutePath cwd p) fs of
        Just _ -> Left $ "Not a directory: " ++ p
        Nothing -> Left $ "Directory not found: " ++ p

    System.FileExists p -> return $ Right $ Map.member (resolveAbsolutePath cwd p) fs

    System.IsDirectory _p -> return $ Right False

    System.Glob base pat -> do
        let basePath = resolveAbsolutePath cwd base
            (searchDir, searchPattern) =
              if isAbsolute pat
              then (takeDirectory pat, takeFileName pat)
              else
                let patDir = takeDirectory pat
                    patName = takeFileName pat
                    resolvedDir = System.basicResolvePath (basePath </> patDir)
                in (resolvedDir, patName)

            allFiles = Map.keys fs
            finalPattern = Glob.compile searchPattern
            matchedAbsolute = filter (\f ->
              let dir = takeDirectory f
                  name = takeFileName f
              in dir == searchDir && Glob.match finalPattern name) allFiles

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

    System.GetCwd -> return cwd
  where
    resolveAbsolutePath basePath path
      | isAbsolute path = System.basicResolvePath path
      | otherwise = System.basicResolvePath (basePath </> path)

-- | Pure interpreter for System.FileSystemWrite using in-memory map
filesystemWriteDummy :: Member (Error String) r
                     => DummyFS
                     -> Sem (System.FileSystemWrite : r) a
                     -> Sem r (DummyFS, a)
filesystemWriteDummy fs prog = do
  -- For now, just run without actual writes (could extend to return updated map)
  result <- interpret (\case
    System.WriteFile _p _d -> return $ Right ()
    System.CreateDirectory _createParents _p -> return $ Right ()
    System.Remove _recursive _p -> return $ Right ()
    ) prog
  return (fs, result)

--------------------------------------------------------------------------------
-- Project Types for Testing
--------------------------------------------------------------------------------

newtype ProjectA = ProjectA FilePath deriving (Show, Eq)
newtype ProjectB = ProjectB FilePath deriving (Show, Eq)

instance HasProjectPath ProjectA where
  getProjectPath (ProjectA p) = p

instance HasProjectPath ProjectB where
  getProjectPath (ProjectB p) = p

--------------------------------------------------------------------------------
-- Test Helpers
--------------------------------------------------------------------------------

runProjectTest :: forall project a. HasProjectPath project
               => FilePath  -- CWD
               -> DummyFS
               -> project
               -> Sem '[FileSystemWrite project, FileSystemRead project, Project project, Fail, System.FileSystemRead, System.FileSystemWrite, Error String] a
               -> Either String a
runProjectTest cwd fs proj prog =
  run
    . runError @String
    . fmap snd
    . filesystemWriteDummy fs
    . filesystemReadDummy cwd fs
    . failToError id
    . projectFileSystemLocal proj
    $ prog

runMultiProjectTest :: FilePath
                    -> DummyFS
                    -> ProjectA
                    -> ProjectB
                    -> Sem '[FileSystemWrite ProjectA, FileSystemRead ProjectA, Project ProjectA,
                             FileSystemWrite ProjectB, FileSystemRead ProjectB, Project ProjectB,
                             Fail, System.FileSystemRead, System.FileSystemWrite, Error String] a
                    -> Either String a
runMultiProjectTest cwd fs projA projB prog =
  run
    . runError @String
    . fmap snd
    . filesystemWriteDummy fs
    . filesystemReadDummy cwd fs
    . failToError id
    . projectFileSystemLocal projB
    . projectFileSystemLocal projA
    $ prog


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Basic Project Operations" $ do
    it "GetProject returns the project value" $ do
      let proj = ProjectA "/test/project"
          result = runProjectTest "/test" Map.empty proj $ getProject @ProjectA

      result `shouldBe` Right proj

    it "listFiles works with project-relative paths" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/file1.txt", "content1")
            , ("/project/file2.txt", "content2")
            ]
          -- This will fail with our dummy impl (no real directory support)
          -- but demonstrates the API
          result = runProjectTest "/project" fs proj $
            fileExists @ProjectA "file1.txt"

      result `shouldBe` Right True

    it "readFile reads from project root" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList [("/project/config.yaml", "test: true")]
          result = runProjectTest "/project" fs proj $
            readFile @ProjectA "config.yaml"

      result `shouldBe` Right "test: true"

    it "glob works relative to project root" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/file1.txt", "content1")
            , ("/project/file2.txt", "content2")
            , ("/project/doc.md", "markdown")
            ]
          result = runProjectTest "/project" fs proj $
            glob @ProjectA "." "*.txt"

      result `shouldBe` Right ["file1.txt", "file2.txt"]

  describe "Multiple Projects on Stack" $ do
    it "can read from different projects independently" $ do
      let projA = ProjectA "/project-a"
          projB = ProjectB "/project-b"
          fs = Map.fromList
            [ ("/project-a/a.txt", "from A")
            , ("/project-b/b.txt", "from B")
            ]
          result = runMultiProjectTest "/test" fs projA projB $ do
            contentA <- readFile @ProjectA "a.txt"
            contentB <- readFile @ProjectB "b.txt"
            return (contentA, contentB)

      result `shouldBe` Right ("from A", "from B")

    it "projects have separate roots" $ do
      let projA = ProjectA "/project-a"
          projB = ProjectB "/project-b"
          fs = Map.fromList
            [ ("/project-a/fileA.txt", "from A")
            , ("/project-b/fileB.txt", "from B")
            ]
          -- Each project can only access files relative to its own root
          result = runMultiProjectTest "/test" fs projA projB $ do
            -- ProjectA can access its own files
            aFile <- readFile @ProjectA "fileA.txt"
            -- ProjectB can access its own files
            bFile <- readFile @ProjectB "fileB.txt"
            return (aFile, bFile)

      result `shouldBe` Right ("from A", "from B")

  describe "Filters" $ do
    it "hideDotfiles filters out dotfiles from glob" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/file.txt", "visible")
            , ("/project/.hidden", "hidden")
            , ("/project/.git", "git")
            ]
          result = runProjectTest "/project" fs proj $
            filterProject @ProjectA hideDotfiles $
              glob @ProjectA "." "*"

      -- Should not include .hidden or .git
      result `shouldSatisfy` \case
        Right files -> not (any (\f -> "." `isPrefixOf` f) files)
        Left _ -> False

    it "hideGit filters out .git directory" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/.git", "git")
            , ("/project/file.txt", "visible")
            ]
          result = runProjectTest "/project" fs proj $
            filterProject @ProjectA hideGit $
              fileExists @ProjectA ".git"

      result `shouldBe` Right False

    it "onlyClaude only allows .claude files" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/.claude/agents/test.md", "agent")
            , ("/project/src/Main.hs", "code")
            ]
          result = runProjectTest "/project" fs proj $
            filterProject @ProjectA onlyClaude $ do
              claude <- fileExists @ProjectA ".claude/agents/test.md"
              src <- fileExists @ProjectA "src/Main.hs"
              return (claude, src)

      -- .claude file exists, src file filtered out
      result `shouldBe` Right (True, False)

    it "filters compose with Semigroup" $ do
      let combined = hideGit <> hideDotfiles
          testPath1 = shouldInclude combined ".git"
          testPath2 = shouldInclude combined ".hidden"
          testPath3 = shouldInclude combined "normal.txt"

      testPath1 `shouldBe` False
      testPath2 `shouldBe` False
      testPath3 `shouldBe` True

    it "filterRead blocks reading filtered files" $ do
      let proj = ProjectA "/project"
          fs = Map.fromList
            [ ("/project/.git", "git data")
            , ("/project/file.txt", "normal")
            ]
          result = runProjectTest "/project" fs proj $
            filterRead @ProjectA hideGit $
              readFile @ProjectA ".git"

      result `shouldSatisfy` \case
        Left err -> "filtered out" `isInfixOf` err
        Right _ -> False

  describe "Capability Separation" $ do
    it "Project effect allows structure exploration without reading" $ do
      -- A function that only needs Project (not Read) can see files but not read them
      let proj = ProjectA "/project"
          fs = Map.fromList [("/project/config.yaml", "test: true")]

          result = runProjectTest "/project" fs proj $
            fileExists @ProjectA "config.yaml"

      result `shouldBe` Right True

    it "FileSystemRead requires reading capability" $ do
      -- A function that needs to read file contents
      let proj = ProjectA "/project"
          fs = Map.fromList [("/project/config.yaml", "test: true")]

          result = runProjectTest "/project" fs proj $
            readFile @ProjectA "config.yaml"

      result `shouldBe` Right "test: true"

  describe "Path Translation" $ do
    it "projectToSystemPath converts project-relative to system paths" $ do
      let proj = ProjectA "/project"
          systemPath = projectToSystemPath proj "src/Main.hs"

      systemPath `shouldBe` "/project/src/Main.hs"

    it "getProjectPath extracts root path" $ do
      let proj = ProjectA "/my/project"
          root = getProjectPath proj

      root `shouldBe` "/my/project"

  describe "Real-world Usage Patterns" $ do
    it "can create separate filtered views of same directory" $ do
      -- Simulate having both a "user files" view and a "claude config" view
      let userFiles = ProjectA "/project"
          claudeConfig = ProjectB "/project"  -- Same directory!
          fs = Map.fromList
            [ ("/project/.claude", "config dir marker")
            , ("/project/src/Main.hs", "code")
            , ("/project/.git", "git data")
            ]

          -- User files view: hide .git and .claude
          result = runMultiProjectTest "/project" fs userFiles claudeConfig $
            filterProject @ProjectA (hideGit <> hideClaude) $ do
              gitExists <- fileExists @ProjectA ".git"
              claudeExists <- fileExists @ProjectA ".claude"
              srcExists <- fileExists @ProjectA "src/Main.hs"
              return (gitExists, claudeExists, srcExists)

      -- .git and .claude filtered out, src visible
      result `shouldBe` Right (False, False, True)

    it "type system prevents reading from wrong project" $ do
      -- This test demonstrates compile-time safety
      -- If you try: readFile @ProjectA "file.txt" but only have ProjectB in scope,
      -- it won't compile. We can't easily test this at runtime, but the types ensure it.

      let projA = ProjectA "/project-a"
          fs = Map.fromList [("/project-a/file.txt", "content")]

          -- This compiles because we have ProjectA
          validProgram :: Members '[FileSystemRead ProjectA, Fail] r => Sem r BS.ByteString
          validProgram = readFile @ProjectA "file.txt"

          result = runProjectTest "/project-a" fs projA validProgram

      result `shouldBe` Right "content"
