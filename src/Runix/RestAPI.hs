{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Runix.RestAPI where
import Polysemy
import Polysemy.Fail
import Data.Aeson
import Data.Kind (Type)
import Data.Time (UTCTime, NominalDiffTime)
import GHC.Stack
import Runix.HTTP
import Runix.Time (Time, Sleep, getCurrentTime, sleepFor)
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.FilePath

newtype Endpoint = Endpoint String
newtype RestData a = RestData a
newtype RestResponse a = RestResponse a

-- | Structured REST error, preserving enough information for retry logic and diagnostics.
data RestError
    = HttpError Int [(String, String)] BSL.ByteString
    -- ^ Non-2xx response: status code, response headers, body
    | ParseError String BSL.ByteString
    -- ^ 2xx response that failed JSON decoding: message, raw body
    | ConnectionError String
    -- ^ No response received: connection failure message
    deriving (Show)

-- | Collapse a structured error to a human-readable string.
-- Use this in contexts that only care about failure, not the cause.
restErrorMessage :: RestError -> String
restErrorMessage (HttpError code _ body) =
    "HTTP error " <> show code <> ": " <> BSL.unpack body
restErrorMessage (ParseError msg _) =
    "Failed to parse response: " <> msg
restErrorMessage (ConnectionError msg) =
    "Connection error: " <> msg

-- | Basic REST API effect for non-streaming requests.
-- The effect returns Either RestError s — errors are results, not failures.
-- Use the convenience functions (post, get, etc.) which call fail on Left,
-- or handle the Either directly when you need structured error information
-- (e.g. for retry logic on HttpError 429).
data RestAPI p (m :: Type -> Type) a where
    RestRequest :: (ToJSON r, FromJSON s) => String -> Endpoint -> Maybe r -> RestAPI p m (Either RestError s)

makeSem ''RestAPI

-- Convenience functions: send the request and fail on error
get :: forall p s r. (FromJSON s, Member (RestAPI p) r, Member Fail r) => Endpoint -> Sem r s
get endpoint = restRequest "GET" endpoint (Nothing :: Maybe ()) >>= either (fail . restErrorMessage) return

post :: forall p req s r. (ToJSON req, FromJSON s, Member (RestAPI p) r, Member Fail r) => Endpoint -> req -> Sem r s
post endpoint body = restRequest "POST" endpoint (Just body) >>= either (fail . restErrorMessage) return

put :: forall p req s r. (ToJSON req, FromJSON s, Member (RestAPI p) r, Member Fail r) => Endpoint -> req -> Sem r s
put endpoint body = restRequest "PUT" endpoint (Just body) >>= either (fail . restErrorMessage) return

delete :: forall p s r. (FromJSON s, Member (RestAPI p) r, Member Fail r) => Endpoint -> Sem r s
delete endpoint = restRequest "DELETE" endpoint (Nothing :: Maybe ()) >>= either (fail . restErrorMessage) return

patch :: forall p req s r. (ToJSON req, FromJSON s, Member (RestAPI p) r, Member Fail r) => Endpoint -> req -> Sem r s
patch endpoint body = restRequest "PATCH" endpoint (Just body) >>= either (fail . restErrorMessage) return

class RestEndpoint p where
    apiroot :: p -> String
    authheaders :: p -> [(String, String)]
    -- | User-Agent header value for this client
    -- Default is "runix/0.1" but applications should override to identify themselves
    useragent :: p -> String
    useragent _ = "runix/0.1"

-- | Build an HTTPRequest from RestEndpoint configuration
makeHTTPRequest :: (RestEndpoint p, ToJSON r) => p -> String -> Endpoint -> Maybe r -> HTTPRequest
makeHTTPRequest api method (Endpoint endpoint) body =
    HTTPRequest
        { method = method
        , uri = apiroot api </> endpoint
        , headers = ("Content-Type", "application/json")
                  : ("User-Agent", useragent api)
                  : authheaders api
        , body = fmap encode body
        }

-- | Basic REST API interpreter (non-streaming)
-- Never fails — HTTP and parse errors are returned as Left RestError.
restapiHTTP :: HasCallStack => (RestEndpoint p, Member HTTP r) => p -> Sem (RestAPI p : r) a -> Sem r a
restapiHTTP api = interpret $ \case
    RestRequest method e maybeData -> do
        let httpReq = makeHTTPRequest api method e maybeData
        result <- send (HttpRequest httpReq)
        case result of
            Left err -> return $ Left $ ConnectionError err
            Right response -> return $ parseResponse response
  where
    parseResponse :: FromJSON s => HTTPResponse -> Either RestError s
    parseResponse response =
        if response.code >= 200 && response.code < 300
        then case decode response.body of
            Just result -> Right result
            Nothing -> Left $ ParseError "JSON decode failed" response.body
        else Left $ HttpError response.code response.headers response.body

-- ============================================================================
-- Resilience
-- ============================================================================

-- | Environment available to the retry function on each attempt.
data RetryEnv = RetryEnv
    { requestStarted :: UTCTime
    , requestEnded   :: UTCTime
    } deriving (Show)

-- | Decision returned by the retry function after each attempt.
data RetryDecision
    = PassThrough       -- ^ Accept the result and return it, regardless of success or failure.
    | RetryImmediately  -- ^ Discard the result and retry at once.
    | RetryAfter NominalDiffTime  -- ^ Discard the result and retry after sleeping.
    deriving (Show)

-- | Resilience interceptor for RestAPI.
--
-- Wraps any 'RestAPI p' effect in the stack, retrying requests according to
-- the supplied function. The function receives its own accumulator (for
-- counting attempts, tracking backoff state, etc.), the timing environment
-- for the attempt, and the raw result — and returns a decision plus the
-- updated accumulator.
--
-- The wrapper never modifies requests or results: it can only affect
-- *when* a request is made, not *what* is sent or returned.
--
-- > withRestAPIResilience (3 :: Int) policy action
-- >   where
-- >     policy n env result
-- >       | n <= 0    = (PassThrough, n)
-- >       | otherwise = case result of
-- >           Left (HttpError 429 _ _) -> (RetryAfter 1, n - 1)
-- >           Left (ConnectionError _) -> (RetryImmediately, n - 1)
-- >           _                        -> (PassThrough, n)
withRestAPIResilience
    :: forall p acc r a.
       Members '[RestAPI p, Time, Sleep] r
    => acc
    -> (forall s. acc -> RetryEnv -> Either RestError s -> (RetryDecision, acc))
    -> Sem r a
    -> Sem r a
withRestAPIResilience initialAcc policy = intercept $ \case
    RestRequest method endpoint maybeData -> attempt initialAcc
      where
        attempt acc = do
            started <- getCurrentTime
            result  <- send (RestRequest method endpoint maybeData)
            ended   <- getCurrentTime
            let env = RetryEnv { requestStarted = started, requestEnded = ended }
                (decision, acc') = policy acc env result
            case decision of
                PassThrough      -> return result
                RetryImmediately -> attempt acc'
                RetryAfter dt    -> sleepFor dt >> attempt acc'

-- | Simple retry combinator for the common case: retry on errors, pass through on success.
--
-- The predicate returns 'Nothing' to give up immediately, or 'Just dt' to retry
-- after 'dt' seconds (use 0 for immediate retry). The 'Int' accumulator counts
-- remaining attempts — when it reaches zero the error is passed through regardless.
--
-- > withRestAPIResilience 3 (retryOnError isTransient) action
-- >   where
-- >     isTransient (HttpError 429 headers _) = Just $ retryAfterHeader headers
-- >     isTransient (ConnectionError _)        = Just 0
-- >     isTransient _                          = Nothing
-- | Simple retry combinator: predicate decides whether to retry and with what backoff.
-- Successful responses always pass through. The 'Int' accumulator counts remaining attempts.
--
-- > withRestAPIResilience 3 (retryOnError isTransient) action
retryOnError
    :: Int                                  -- ^ Maximum number of retries
    -> (RestError -> Maybe NominalDiffTime) -- ^ Nothing = give up, Just dt = retry after dt (0 = immediately)
    -> Int -> RetryEnv -> Either RestError s -> (RetryDecision, Int)
retryOnError _   _    0   _ _          = (PassThrough, 0)
retryOnError _   _    acc _ (Right _)  = (PassThrough, acc)
retryOnError _   pred acc _ (Left err) =
    case pred err of
        Nothing -> (PassThrough, acc)
        Just dt -> (if dt <= 0 then RetryImmediately else RetryAfter dt, acc - 1)

-- | Like 'retryOnError', but the predicate also receives the current attempt number
-- (0-indexed), allowing backoff to scale with the number of retries already made.
--
-- > withRestAPIResilience 10 (retryOnErrorN 10 exponentialBackoff) action
retryOnErrorN
    :: Int                                         -- ^ Maximum number of retries
    -> (Int -> RestError -> Maybe NominalDiffTime) -- ^ attempt -> error -> backoff
    -> Int -> RetryEnv -> Either RestError s -> (RetryDecision, Int)
retryOnErrorN _   _    0   _ _          = (PassThrough, 0)
retryOnErrorN _   _    acc _ (Right _)  = (PassThrough, acc)
retryOnErrorN max pred acc _ (Left err) =
    let attempt = max - acc
    in case pred attempt err of
        Nothing -> (PassThrough, acc)
        Just dt -> (if dt <= 0 then RetryImmediately else RetryAfter dt, acc - 1)

-- | Backoff predicate: retries on 429 (honouring Retry-After header) and connection
-- errors (with exponential backoff capped at 60s). All other errors pass through.
--
-- Intended as the default predicate for LLM provider requests.
llmRetryBackoff :: Int -> RestError -> Maybe NominalDiffTime
llmRetryBackoff attempt (HttpError 429 headers _) =
    Just $ max (exponentialDelay attempt) (maybe 0 fromIntegral (retryAfterHeader headers))
llmRetryBackoff attempt (ConnectionError _) =
    Just $ exponentialDelay attempt
llmRetryBackoff _ _ = Nothing

-- | Parse the Retry-After header as an integer number of seconds.
retryAfterHeader :: [(String, String)] -> Maybe Int
retryAfterHeader headers =
    lookup "retry-after" headers >>= \v -> case reads v of
        [(n, "")] -> Just n
        _         -> Nothing

-- | 2^attempt seconds, capped at 60.
exponentialDelay :: Int -> NominalDiffTime
exponentialDelay attempt = min 60 (fromIntegral (2 ^ attempt :: Int))

-- | Default resilience wrapper for LLM provider requests.
-- 10 retries with exponential backoff, honouring Retry-After headers on 429s.
llmRetry :: Members '[RestAPI p, Time, Sleep] r => Sem r a -> Sem r a
llmRetry = withRestAPIResilience 10 (retryOnErrorN 10 llmRetryBackoff)

-- | Generic resilience wrapper for any action returning 'Either RestError a'.
--
-- Identical policy model to 'withRestAPIResilience' but works on a plain action
-- rather than an intercepted effect. Use this for streaming paths where the
-- entire stream lifecycle (start → fetch → close) is the unit of retry.
withResilience
    :: forall acc r a.
       Members '[Time, Sleep] r
    => acc
    -> (forall s. acc -> RetryEnv -> Either RestError s -> (RetryDecision, acc))
    -> Sem r (Either RestError a)
    -> Sem r (Either RestError a)
withResilience initialAcc policy action = attempt initialAcc
  where
    attempt acc = do
        started <- getCurrentTime
        result  <- action
        ended   <- getCurrentTime
        let env = RetryEnv { requestStarted = started, requestEnded = ended }
            (decision, acc') = policy acc env result
        case decision of
            PassThrough      -> return result
            RetryImmediately -> attempt acc'
            RetryAfter dt    -> sleepFor dt >> attempt acc'

-- | Default resilience wrapper for LLM streaming requests.
-- Same policy as 'llmRetry': 10 retries, exponential backoff, Retry-After header honoured.
llmStreamRetry :: Members '[Time, Sleep] r => Sem r (Either RestError a) -> Sem r (Either RestError a)
llmStreamRetry = withResilience 10 (retryOnErrorN 10 llmRetryBackoff)
