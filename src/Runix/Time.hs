{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Runix.Time where

import Polysemy
import Data.Time (UTCTime, NominalDiffTime)
import qualified Data.Time as Time
import Data.Kind (Type)
import Control.Concurrent (threadDelay)

-- | Time effect for getting current time
data Time (m :: Type -> Type) a where
    GetCurrentTime :: Time m UTCTime

makeSem ''Time

-- | IO interpreter for Time
timeIO :: Member (Embed IO) r => Sem (Time : r) a -> Sem r a
timeIO = interpret $ \case
    GetCurrentTime -> embed Time.getCurrentTime

-- | Sleep effect for suspending execution for a duration
data Sleep (m :: Type -> Type) a where
    SleepFor :: NominalDiffTime -> Sleep m ()

makeSem ''Sleep

-- | IO interpreter for Sleep
sleepIO :: Member (Embed IO) r => Sem (Sleep : r) a -> Sem r a
sleepIO = interpret $ \case
    SleepFor dt -> embed $ threadDelay $ round $ dt * 1000000

-- | No-op interpreter for Sleep (for tests)
sleepNoop :: Sem (Sleep : r) a -> Sem r a
sleepNoop = interpret $ \case
    SleepFor _ -> return ()
