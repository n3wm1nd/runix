{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Runix.Cancellation.Effects
  ( Cancellation(..)
  , isCanceled
  , cancelNoop
  ) where

import Prelude
import Polysemy
import Data.Kind (Type)

-- | Cancellation effect for stopping long-running operations
-- This is NOT a failure - it's a signal to stop where we are and return control
-- Partial results are fine and stay in history
--
-- The cancellation flag is set externally (e.g., by the UI thread) via STM,
-- and this effect just checks if cancellation was requested.
data Cancellation (m :: Type -> Type) a where
  IsCanceled :: Cancellation m Bool

makeSem ''Cancellation

-- | Noop interpreter: cancellation is never set (for non-interactive clients)
-- This replicates the current behavior where cancellation doesn't exist
cancelNoop :: Sem (Cancellation : r) a -> Sem r a
cancelNoop = interpret $ \case
  IsCanceled -> return False
