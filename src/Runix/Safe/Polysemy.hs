{-# LANGUAGE Trustworthy #-}

-- | Trustworthy re-export of Polysemy for Safe Haskell
--
-- Polysemy itself isn't marked Safe/Trustworthy, so we provide
-- this trustworthy wrapper for use in Safe modules.
module Runix.Safe.Polysemy
  ( module Polysemy
  ) where

import Polysemy
