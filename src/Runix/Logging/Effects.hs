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

data Logging (m :: Type -> Type) a where
    Info :: HasCallStack => CallStack -> Text -> Logging m ()
    Warning :: HasCallStack => CallStack -> Text -> Logging m ()
    Error :: HasCallStack => CallStack -> Text -> Logging m ()
--makeSem ''Logging
info :: HasCallStack => Member Logging r => Text -> Sem r ()
info t = withFrozenCallStack $ send (Info callStack t) 
warning :: HasCallStack => Member Logging r => Text -> Sem r ()
warning t = withFrozenCallStack $ send (Warning callStack t)
error :: HasCallStack => Member Logging r => Text -> Sem r ()
error t = withFrozenCallStack $ send (Runix.Logging.Effects.Error callStack t)

