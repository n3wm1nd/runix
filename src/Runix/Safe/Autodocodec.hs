{-# LANGUAGE Trustworthy #-}

-- | Trustworthy re-export of Autodocodec for Safe Haskell
--
-- Autodocodec itself isn't marked Safe/Trustworthy, so we provide
-- this trustworthy wrapper for use in Safe modules.
module Runix.Safe.Autodocodec
  ( module Autodocodec
  ) where

import Autodocodec
