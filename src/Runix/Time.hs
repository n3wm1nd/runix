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
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Kind (Type)

-- | Time effect for getting current time
data Time (m :: Type -> Type) a where
    GetCurrentTime :: Time m UTCTime

makeSem ''Time

-- | IO interpreter for Time
timeIO :: Member (Embed IO) r => Sem (Time : r) a -> Sem r a
timeIO = interpret $ \case
    GetCurrentTime -> embed Time.getCurrentTime
