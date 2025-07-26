{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding(error)
import Runix.Runner
import Runix.Effects hiding (main)
import GHC.Stack
import Polysemy
import qualified Data.Text as T

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

x :: Member LLM r => Sem r T.Text
x = do
    askLLM "what is the answer to life, the universe, and everything?"

rx :: IO (Either String T.Text)
rx = do
    runUntrusted x