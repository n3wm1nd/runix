{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}


module Runix.Runner (runUntrusted) where
import Prelude hiding (readFile, writeFile, error)
import qualified Prelude

import Polysemy
import Polysemy.Fail
import Polysemy.Error

import Runix.Effects
import qualified Runix.Compiler as Compiler
import Data.Either (fromRight)

-- Engine
type SafeEffects = [FileSystem, RestAPI, CompileTask]

filesystemIO :: Members [Embed IO, Logging] r => Sem (FileSystem : r) a -> Sem r a
filesystemIO = interpret $ \case
    ReadFile p -> do
        info $ "reading file: " ++ p
        embed $ Prelude.readFile p
    WriteFile p d -> do
        info $ "writing file: " ++ p
        embed $ Prelude.writeFile p d

restapiIO :: Member (Embed IO) r => Sem (RestAPI : r) a -> Sem r a
restapiIO = interpret $ \case
    RestPost _e _d -> undefined

compileTaskIO :: Members [ Embed IO, Logging, FileSystem ] r => Sem (CompileTask : r) a -> Sem r a
compileTaskIO = interpret $ \case
    CompileTask project -> do 
        info $ "compiling haskell code: " <> project.name
        result <- embed $ Compiler.compile project
        case result of
            -- FIXME show errors and warnings in info/warning log
            f@CompileFail {} -> do
                warning "compilation failure"
                info $ show f.compileWarnings
                warning $ show f.compileErrors
                return f
            s@CompileSuccess {} -> do
                info "compilation success"
                info $ show s.compileWarnings
                return s


loggingIO :: Member (Embed IO) r => Sem (Logging : r) a -> Sem r a
loggingIO = interpret $ \case
    Info m -> embed $ putStrLn $ "info: " ++ m
    Warning m -> embed $ putStrLn $ "warn: " ++ m
    Error m -> embed $ putStrLn $ " err: " ++ m

loggingNull :: Sem (Logging : r) a -> Sem r a
loggingNull = interpret $ \case
    Info _ -> pure ()
    Warning _ -> pure ()
    Error _ -> pure ()

failLog :: Members [Logging, Error String] r => Sem (Fail : r) a -> Sem r a
failLog = interpret $ \(Fail e) -> error e >> throw e

runUntrusted :: (forall r . Members SafeEffects r => Sem r a) -> IO (Either String a)
runUntrusted = runM . runError . loggingIO . failLog . filesystemIO. restapiIO. compileTaskIO

x :: Member FileSystem r => Sem r ()
x = do
    _ <- readFile "."
    return ()

rx :: IO ()
rx = do
    res <- runUntrusted x
    return $ fromRight () res
