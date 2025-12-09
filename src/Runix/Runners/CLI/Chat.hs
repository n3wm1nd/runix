{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Runix.Runners.CLI.Chat (chatLoop) where

import Prelude hiding (readFile, writeFile, error, getLine)
import System.IO (hFlush, stdout, hIsEOF, stdin)
import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Polysemy
import UniversalLLM (Message)


-- | Interactive chat loop that accumulates conversation history
-- The agent function receives user input and current history, returns updated history
chatLoop :: Member (Embed IO) r
         => (T.Text -> [Message model] -> Sem r [Message model])
         -> [Message model]
         -> Sem r ()
chatLoop agentFunc history = do
    -- Show prompt first
    embed $ T.putStr "> "
    embed $ hFlush stdout

    -- Read user input
    isEOF <- embed $ hIsEOF stdin
    unless isEOF $ do
        userInput <- embed T.getLine

        unless (T.null $ T.strip userInput) $ do
            -- Call the agent function with user input and current history
            updatedHistory <- agentFunc userInput history
            -- Continue with updated history
            chatLoop agentFunc updatedHistory

        -- Continue loop even if input was empty
        chatLoop agentFunc history