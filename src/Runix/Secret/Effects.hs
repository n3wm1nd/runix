{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Runix.Secret.Effects where
import Polysemy
import Data.Kind (Type)

data (Secret s) (m :: Type -> Type) a where
    GetSecret :: (Secret s) m s
makeSem ''Secret

runSecret :: Sem r s -> Sem (Secret s:r) a -> Sem r a
runSecret s = interpret $ \case
    GetSecret -> s
