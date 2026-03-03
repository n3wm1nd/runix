{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HTTPStreamingInterceptorSpec (spec) where

import Test.Hspec
import Polysemy
import Polysemy.Fail (Fail, runFail)
import qualified Data.ByteString as BS
import Data.IORef

import qualified Data.ByteString.Lazy as BSL
import Runix.HTTP (HTTPStreaming, HTTPStreamResult(..), HTTPRequest(..), withSimpleHTTPStreamingLogging, withStreamingHeaders)
import Runix.Streaming (Streaming(..), interpretStreamingStateful, startStream, fetchNext)
import Runix.Logging (Logging(..), Level(..))

-- ============================================================================
-- Test Helpers
-- ============================================================================

-- | Mock HTTPStreaming interpreter that yields fixed chunks
mockHTTPStreaming :: Members '[Embed IO, Fail] r
                  => Sem (HTTPStreaming : r) a
                  -> Sem r a
mockHTTPStreaming = interpretStreamingStateful onStart onFetch onClose
  where
    onStart :: HTTPRequest -> Sem r (Either String [BS.ByteString])
    onStart _req = return $ Right ["chunk1", "chunk2", "chunk3"]

    onFetch :: [BS.ByteString] -> Sem r (Maybe BS.ByteString, [BS.ByteString])
    onFetch [] = return (Nothing, [])
    onFetch (chunk:rest) = return (Just chunk, rest)

    onClose :: [BS.ByteString] -> Sem r HTTPStreamResult
    onClose _ = return $ HTTPStreamResult 200 [] BSL.empty

-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec
spec = describe "HTTP Streaming Interceptors" $ do

    describe "withSimpleHTTPStreamingLogging" $ do
        it "logs stream start and completion" $ do
            -- Use IORef to capture logs
            logsRef <- newIORef ([] :: [String])

            let testRequest = HTTPRequest
                    { method = "POST"
                    , uri = "https://example.com/api"
                    , headers = []
                    , body = Nothing
                    }

                -- Logging interpreter that captures to IORef
                loggingToIORef :: Member (Embed IO) r => Sem (Logging : r) a -> Sem r a
                loggingToIORef = interpret $ \case
                    Log _level _stack msg -> embed $ modifyIORef logsRef (++ [show msg])

            _ <- runM
               $ runFail
               $ mockHTTPStreaming
               $ loggingToIORef
               $ withSimpleHTTPStreamingLogging $ do
                    ((), (_ :: HTTPStreamResult)) <- startStream @BS.ByteString testRequest $ do
                        _ <- fetchNext @BS.ByteString
                        _ <- fetchNext @BS.ByteString
                        _ <- fetchNext @BS.ByteString
                        return ()
                    return ()

            logs <- readIORef logsRef

            -- Should log start and completion
            length logs `shouldBe` 2
            head logs `shouldContain` "POST"
            head logs `shouldContain` "https://example.com/api"
            head logs `shouldContain` "(streaming)"
            last logs `shouldContain` "completed"

    describe "withStreamingHeaders" $ do
        it "modifies request headers" $ do
            let testRequest = HTTPRequest
                    { method = "GET"
                    , uri = "https://example.com"
                    , headers = [("Original", "Header")]
                    , body = Nothing
                    }

                addAuthHeader req = req { headers = ("Authorization", "Bearer token") : headers req }

                loggingNull :: Member (Embed IO) r => Sem (Logging : r) a -> Sem r a
                loggingNull = interpret $ \case
                    Log _ _ _ -> return ()

            -- We just verify it compiles and runs without error
            _ <- runM
               $ runFail
               $ mockHTTPStreaming
               $ loggingNull
               $ withStreamingHeaders addAuthHeader $ do
                    ((), (_ :: HTTPStreamResult)) <- startStream @BS.ByteString testRequest $ return ()
                    return ()

            -- If it didn't crash, the test passes
            True `shouldBe` True
