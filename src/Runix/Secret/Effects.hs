{-# LANGUAGE Trustworthy #-}
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
import Polysemy.Fail
import Data.Kind (Type)
import System.Environment (lookupEnv)

data (Secret s) (m :: Type -> Type) a where
    GetSecret :: (Secret s) m s
makeSem ''Secret

runSecret :: Sem r s -> Sem (Secret s:r) a -> Sem r a
runSecret s = interpret $ \case
    GetSecret -> s

-- | Read a secret from an environment variable
secretEnv :: Members [Fail, Embed IO] r => (String -> s) -> String -> Sem (Secret s :r) a -> Sem r a
secretEnv gensecret envname = interpret $ \case
    GetSecret -> do
        mk <- embed $ lookupEnv envname
        case mk of
            Nothing -> fail $ "secretEnv: ENV " <> envname <> " is unset"
            Just key -> pure $ gensecret key
