{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Generic streaming abstraction for pull-based streaming with results.
--
-- This module provides a generic streaming effect that can be used for any
-- pull-based streaming scenario. Like conduits, streams have:
-- - An item type (what gets streamed/yielded)
-- - A result type (what gets returned when the stream completes)
--
-- The pattern used here (StreamId-based with internal/public split) allows
-- the interpreter to manage multiple concurrent streams while providing
-- a clean bracket-based API to users.
module Runix.Streaming
  ( -- * Generic streaming effect
    Streaming(..)
  , StreamResult(..)
  , StreamId(..)
    -- * Operations
  , fetchNext
  , startStream
    -- * Helpers for implementing interpreters
  , interpretStreamingStateful
  , StreamsState(..)
  ) where

import Polysemy
import Polysemy.Fail
import Polysemy.State
import Data.Kind (Type)
import qualified Data.Map.Strict as Map

-- | Stream identifier for managing concurrent streams
newtype StreamId = StreamId Int
    deriving (Eq, Ord, Show)

-- | Internal streaming effect for managing streams
--
-- Users don't interact with this directly - use StreamResult instead.
-- The config type is determined by the specific streaming implementation.
data Streaming item result config (m :: Type -> Type) a where
    StartStream :: config -> Streaming item result config m (Either String StreamId)
    FetchItem :: StreamId -> Streaming item result config m (Maybe item)
    CloseStream :: StreamId -> Streaming item result config m result

-- | Public effect for consuming streams
--
-- Provided by startStream which manages the stream lifecycle.
data StreamResult item (m :: Type -> Type) a where
    FetchNext :: StreamResult item m (Maybe item)

-- | Fetch the next item from the stream
fetchNext :: Member (StreamResult item) r => Sem r (Maybe item)
fetchNext = send FetchNext

-- | Start a stream with bracket-style resource management
--
-- Provides StreamResult effect for the duration of the action.
-- Returns both the action result and the stream's final result.
-- Automatically closes the stream when done.
startStream :: forall item result config r a.
               Members '[Streaming item result config, Fail] r
            => config
            -> Sem (StreamResult item : r) a
            -> Sem r (a, result)
startStream config action = do
    -- Start the stream, get StreamId
    result <- send (StartStream config)
    case result of
        Left err -> fail err
        Right streamId -> do
            -- Interpret StreamResult by forwarding to Streaming with streamId
            actionResult <- interpret (\case
                FetchNext -> send (FetchItem streamId)
                ) action
            -- Close stream and get final result
            streamResult <- send (CloseStream streamId)
            return (actionResult, streamResult)

-- ============================================================================
-- Helpers for implementing interpreters
-- ============================================================================

-- | State for managing multiple concurrent streams
data StreamsState streamState = StreamsState
    { nextStreamId :: Int
    , activeStreams :: Map.Map StreamId streamState
    }

-- | Helper for implementing Streaming interpreters using State
--
-- This abstracts the common pattern of:
-- - Tracking StreamId -> streamState in a State map
-- - Allocating new StreamIds
-- - Looking up and managing stream state
--
-- You provide three functions:
-- - onStart: config -> creates initial stream state (in any effects)
-- - onFetch: streamState -> fetches next item and updates state
-- - onClose: streamState -> closes stream and returns final result
interpretStreamingStateful
    :: forall item result config streamState r a.
       Member Fail r
    => (config -> Sem r (Either String streamState))  -- ^ Initialize stream state
    -> (streamState -> Sem r (Maybe item, streamState))  -- ^ Fetch item and update state
    -> (streamState -> Sem r result)  -- ^ Close stream and get result
    -> Sem (Streaming item result config : r) a
    -> Sem r a
interpretStreamingStateful onStart onFetch onClose action =
    evalState (StreamsState 0 Map.empty) $
    interpret (\case
        StartStream config -> do
            -- Initialize stream state using provided function
            result <- raise $ onStart config
            case result of
                Left err -> return $ Left err
                Right initialState -> do
                    -- Allocate new StreamId
                    streamsState <- get @(StreamsState streamState)
                    let streamId = StreamId (nextStreamId streamsState)
                    put $ streamsState
                        { nextStreamId = nextStreamId streamsState + 1
                        , activeStreams = Map.insert streamId initialState (activeStreams streamsState)
                        }
                    return $ Right streamId

        FetchItem streamId -> do
            -- Look up stream state
            streamsState <- get @(StreamsState streamState)
            case Map.lookup streamId (activeStreams streamsState) of
                Nothing -> return Nothing  -- Stream already closed
                Just streamState -> do
                    -- Fetch using provided function
                    (mItem, streamState') <- raise $ onFetch streamState
                    -- Update stream state
                    modify $ \ss -> ss { activeStreams = Map.insert streamId streamState' (activeStreams ss) }
                    return mItem

        CloseStream streamId -> do
            -- Look up and remove stream state
            streamsState <- get @(StreamsState streamState)
            case Map.lookup streamId (activeStreams streamsState) of
                Nothing -> raise $ fail "Stream not found"
                Just streamState -> do
                    -- Remove from map
                    modify $ \ss -> ss { activeStreams = Map.delete streamId (activeStreams ss) }
                    -- Close using provided function
                    raise $ onClose streamState

    ) (raiseUnder @(State (StreamsState streamState)) action)
