{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Runix.Streaming.Effects where

import Polysemy
import Polysemy.State
import Data.Kind (Type)

-- | Effect for emitting streaming chunks
-- This allows interpreters to emit intermediate results during processing
data StreamChunk chunk (m :: Type -> Type) a where
    EmitChunk :: chunk -> StreamChunk chunk m ()

makeSem ''StreamChunk

-- | Ignore all streaming chunks (default behavior for non-streaming use cases)
ignoreChunks :: Sem (StreamChunk chunk : r) a -> Sem r a
ignoreChunks = interpret $ \case
    EmitChunk _ -> return ()

-- | Collect all emitted chunks along with the final result
collectChunks :: Sem (StreamChunk chunk : r) a -> Sem r ([chunk], a)
collectChunks action = do
    (chunks, result) <- runState [] $ reinterpret (\case
        EmitChunk chunk -> modify (chunk :)) action
    return (reverse chunks, result)

-- | Reinterpret chunks with a pure transformation function
-- This allows converting chunks from one type to another in the interpreter stack
-- If the function returns Nothing, the chunk is dropped
mapChunks :: (a -> Maybe b)
          -> Sem (StreamChunk a : r) x
          -> Sem (StreamChunk b : r) x
mapChunks f = reinterpret $ \case
    EmitChunk chunk -> case f chunk of
        Just b -> emitChunk b
        Nothing -> return ()

-- | Reinterpret chunks with a stateful transformation
-- Useful for parsers that need to accumulate state (e.g., SSE parsing)
mapChunksStateful :: s  -- ^ Initial state
                  -> (s -> a -> (s, [b]))  -- ^ State transformer (returns new state and zero or more chunks)
                  -> Sem (StreamChunk a : r) x
                  -> Sem (StreamChunk b : r) x
mapChunksStateful initial f action =
    evalState initial $ reinterpret2 (\case
        EmitChunk chunk -> do
            s <- get
            let (s', bs) = f s chunk
            put s'
            mapM_ (raise . emitChunk) bs
        ) action
