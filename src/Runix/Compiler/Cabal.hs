{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}

module Runix.Compiler.Cabal where

import System.Directory (createDirectoryIfMissing, getCurrentDirectory, withCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp
import System.Process
import System.Exit
-- cabal specific
import Distribution.CabalSpecVersion
import Distribution.ModuleName
import Distribution.PackageDescription.PrettyPrint 
import Distribution.SPDX (License (NONE))
import Distribution.Simple
import Distribution.Types.BuildInfo
import Distribution.Types.Library
import Distribution.Types.PackageDescription
import qualified Distribution.Utils.Path as UPath


data Project = Project {
    name :: PackageName,
    description :: String,
    dependencies :: [Dependency],
    mainmodule :: ModuleSource,
    modules :: [ModuleSource],
    othermodules :: [ModuleSource]
}

emptyProject :: Project
emptyProject = Project {
    name = fromString "project",
    description = "",
    dependencies = [],
    mainmodule = ModuleSource {name = fromString "Main", sourcecode = "module Main where\n"},
    modules = [],
    othermodules = []
}

data ModuleSource = ModuleSource {
    name :: ModuleName,
    sourcecode :: String
}

projectpath :: String -> IO FilePath
projectpath projectname = do
    root <- getCurrentDirectory
    return $ root </> "tasks" </> projectname


-- save a Project in the current directory
saveproject :: Project -> IO FilePath
saveproject project = do
    let desc = packageDescription project
    let name = unPackageName desc.package.pkgName
    let mods = project.mainmodule : project.modules <> project.othermodules
    -- Create project directory structure
    createDirectoryIfMissing True "src"
    -- Save modules
    mapM_ saveModule mods
    writePackageDescription (name <> ".cabal") desc
    getCurrentDirectory


-- Helper function to save a module with proper directory structure
saveModule :: ModuleSource -> IO ()
saveModule ModuleSource{name, sourcecode} = do
    let filePath = "src" </> toFilePath name <> ".hs"
    createDirectoryIfMissing True (takeDirectory filePath)
    writeFile filePath sourcecode

packageDescription :: Project -> PackageDescription
packageDescription project =
  -- generateCabalFile project.name project.description project.dependencies mods omods
  emptyPackageDescription {
    package = PackageIdentifier {pkgName = project.name, pkgVersion = version0},
    description = fromString project.description,
    library = Just lib,
    specVersion = cabalSpecLatest,
    licenseRaw = Left NONE
  }
  where
    modname x = x.name
    mods = map modname (project.mainmodule : project.modules)
    omods = map modname project.othermodules
    maindep = mkDependency (fromString "base") anyVersion mainLibSet
    lib = emptyLibrary {
        exposedModules = mods,
        libBuildInfo = emptyBuildInfo {
            otherModules = omods,
            targetBuildDepends = maindep : project.dependencies,
            -- this is completely fine, but will change with future cabal releases
            hsSourceDirs = [ UPath.unsafeMakeSymbolicPath "src" ]
        }
    }

data RawCompileResult = CompileSuccess {log :: String, err :: String} | CompileError {code :: Int, log :: String, err :: String}
    deriving (Show)

inTempEnvironment :: IO a -> IO a
inTempEnvironment action = withSystemTempDirectory "runix_compile" $ flip withCurrentDirectory action


-- | Compiles a Haskell snippet in temporary environment
-- Returns either build logs or error code with output
compilesnippet :: String -> IO RawCompileResult
compilesnippet src = do
  let mainmodulename = "Test"
  let proj = emptyProject {
    name = fromString "tmpcompile",
    mainmodule = ModuleSource {
        name = fromString mainmodulename, 
        sourcecode = "module " <> mainmodulename <> " where\n\n" <> src
        }
  }
  compile proj

compileproject :: IO RawCompileResult
compileproject = do
    (res, logout, logerr) <- readCreateProcessWithExitCode (shell "cabal build")  ""

    return $ case res of
        ExitSuccess -> CompileSuccess { log = logout, err = logerr }
        ExitFailure n -> CompileError { code = n, log = logout, err = logerr }


compile :: Project -> IO RawCompileResult
compile project = inTempEnvironment $ do
  _projectpath <- saveproject project
  compileproject