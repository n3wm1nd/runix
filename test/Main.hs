{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding(error)
import Runix.Runner
import Runix.Effects hiding (main)
import GHC.Stack
import Polysemy

main :: IO ()
main = putStrLn "Test suite not yet implemented."


y :: HasCallStack => Member Logging r => Sem r ()
y = do 
    info "testlog"
    error "errorlog"

yy :: HasCallStack => Member Logging r => Sem r ()
yy = y

ry :: HasCallStack => IO (Either String ())
ry = do
    runUntrusted yy