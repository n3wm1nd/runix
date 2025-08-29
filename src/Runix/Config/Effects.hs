{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Runix.Config.Effects where 
import Data.Kind (Type)
import Polysemy


data (Config c) (m :: Type -> Type ) a where
    GetConfig :: (Config c) m c
makeSem ''Config

runConfig ::  c -> Sem (Config c:r) a -> Sem r a
runConfig con = interpret $ \case
    GetConfig -> pure con
