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

module Runix.Logging.Effects where
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Data.Kind (Type)
import GHC.Stack
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import Prelude hiding (log, error)

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

-- | IO interpreter for Logging effect
loggingIO :: HasCallStack => Member (Embed IO) r => Sem (Logging : r) a -> Sem r a
loggingIO = interpret $ \v -> do
    case v of
        Log level cs m -> embed $ putStrLn $ prefix level <> l cs m
    where
        prefix Info = "info: "
        prefix Warning = "warn: "
        prefix Error = " err: "
        l cs m = funname (getCallStack cs) <> T.unpack m
        funname (_:f:frames) = (intercalate "." . reverse . map fst) (f:frames) <> ": "
        funname _ = ""

-- | Null interpreter for Logging (discards all logs)
-- NOTE: Currently unused, but kept for future use
loggingNull :: Sem (Logging : r) a -> Sem r a
loggingNull = interpret $ \case
    Log _ _ _ -> pure ()

-- | Interpreter that converts Fail to Logging + Error
failLog :: Members [Logging, Error String] r => Sem (Fail : r) a -> Sem r a
failLog = interpret $ \(Fail e) -> error (T.pack e) >> throw e

