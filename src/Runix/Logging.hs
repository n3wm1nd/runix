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
{-# LANGUAGE TypeApplications #-}

module Runix.Logging where
import Polysemy
import Polysemy.Error
import Polysemy.Fail
import Polysemy.Writer (Writer, tell, runWriter)
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
loggingNull :: Sem (Logging : r) a -> Sem r a
loggingNull = interpret $ \case
    Log _ _ _ -> pure ()

-- | Interpreter that captures logs to a list
-- Returns both the list of log messages and the result
-- Useful for testing
loggingList :: forall r a. Sem (Logging : r) a -> Sem r ([(Level, Text)], a)
loggingList = runWriter . reinterpret @Logging @(Writer [(Level, Text)]) (\case
    Log level _cs msg -> tell [(level, msg)]
    )

-- | Interpreter that converts Fail to Logging + Error
failLog :: Members [Logging, Error String] r => Sem (Fail : r) a -> Sem r a
failLog = interpret $ \(Fail e) -> error (T.pack e) >> throw e

