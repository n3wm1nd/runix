{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Runix.Logging.Effects where
import Polysemy
import Data.Kind (Type)
import GHC.Stack
import Data.Text
import Prelude hiding (log)

data Level = Info | Warning | Error
    deriving (Show, Eq, Ord)

data Logging (m :: Type -> Type) a where
    Log :: HasCallStack => Level -> CallStack -> Text -> Logging m ()

makeSem ''Logging

-- Convenience functions for compatibility
info :: HasCallStack => Member Logging r => Text -> Sem r ()
info t = withFrozenCallStack $ log Info callStack t

warning :: HasCallStack => Member Logging r => Text -> Sem r ()
warning t = withFrozenCallStack $ log Warning callStack t

error :: HasCallStack => Member Logging r => Text -> Sem r ()
error t = withFrozenCallStack $ log Error callStack t

