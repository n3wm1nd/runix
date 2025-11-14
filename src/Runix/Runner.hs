{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}


module Runix.Runner (
    runUntrusted,
    SafeEffects,
    -- Re-exports from other modules
    module Runix.FileSystem.Effects,
    module Runix.Grep.Effects,
    module Runix.Bash.Effects,
    module Runix.Cmd.Effects,
    module Runix.HTTP.Effects,
    module Runix.Logging.Effects,
    module Runix.Secret.Effects,
    module Runix.Compiler.Effects,
    Coding
) where

-- Standard libraries
import System.Environment (lookupEnv)

-- Polysemy libraries
import Polysemy
import Polysemy.Fail
import Polysemy.Error

-- Local modules
import Runix.FileSystem.Effects
import Runix.Grep.Effects
import Runix.Bash.Effects
import Runix.Cmd.Effects
import Runix.HTTP.Effects
import Runix.Logging.Effects
import Runix.Secret.Effects
import Runix.Compiler.Effects
import Runix.Cancellation.Effects (Cancellation, cancelNoop, onCancellation)
import qualified Runix.Compiler.Compiler as Compiler
import Runix.LLM.Interpreter (OpenRouter(..), GenericModel(..), interpretOpenRouter)
import Runix.LLM.Effects

-- External libraries
import qualified Data.Text as T
import GHC.Stack


-- Capability marker typeclass
class Coding model

instance Coding GenericModel

-- Engine - Generic over provider and model
type SafeEffects provider model = [FileSystemRead, FileSystemWrite, HTTP, CompileTask, Logging, LLM provider model]

-- | IO interpreter for CompileTask effect
-- NOTE: This must stay here to avoid circular dependency (Compiler.Compiler imports Compiler.Effects)
compileTaskIO :: HasCallStack => Members [Embed IO, Logging, FileSystemRead, FileSystemWrite] r => Sem (CompileTask : r) a -> Sem r a
compileTaskIO = interpret $ \case
    CompileTask project -> do
        info $ "compiling haskell code: " <> T.pack project.name
        result <- embed $ Compiler.compile project
        case result of
            f@CompileFail {} -> do
                warning "compilation failure"
                info $ T.pack $ show f.compileWarnings
                warning $ T.pack $ show f.compileErrors
                return f
            s@CompileSuccess {} -> do
                info "compilation success"
                info $ T.pack $ show s.compileWarnings
                return s

-- | Example OpenRouter interpreter using environment variable
-- This demonstrates how to compose the various effect interpreters
openrouter :: Members [Embed IO, Fail, HTTP, Cancellation] r => Sem (LLM OpenRouter GenericModel : r) a -> Sem r a
openrouter action = do
    apiKey <- embed $ lookupEnv "OPENROUTER_API_KEY"
    case apiKey of
        Nothing -> fail "OPENROUTER_API_KEY environment variable not set"
        Just key ->
            runSecret (pure key)
            . interpretOpenRouter OpenRouter (GenericModel "deepseek/deepseek-chat-v3-0324:free")
            . raiseUnder
            $ action

-- | Example runner combining all effect interpreters
-- This demonstrates the complete effect stack composition
runUntrusted :: HasCallStack => (forall r . Members (SafeEffects OpenRouter GenericModel) r => Sem r a) -> IO (Either String a)
runUntrusted = runM . runError . loggingIO . failLog . cancelNoop . httpIO_ . filesystemIO . openrouter . compileTaskIO

