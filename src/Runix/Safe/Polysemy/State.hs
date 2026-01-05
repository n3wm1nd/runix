{-# LANGUAGE Trustworthy #-}

-- | Trustworthy re-export of Polysemy.State for Safe Haskell
--
-- Polysemy.State itself isn't marked Safe/Trustworthy, so we provide
-- this trustworthy wrapper for use in Safe modules.
module Runix.Safe.Polysemy.State
  ( module Polysemy.State
  ) where

import Polysemy.State
