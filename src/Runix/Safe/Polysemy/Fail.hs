{-# LANGUAGE Trustworthy #-}

-- | Trustworthy re-export of Polysemy.Fail for Safe Haskell
--
-- Polysemy.Fail itself isn't marked Safe/Trustworthy, so we provide
-- this trustworthy wrapper for use in Safe modules.
module Runix.Safe.Polysemy.Fail
  ( module Polysemy.Fail
  ) where

import Polysemy.Fail
