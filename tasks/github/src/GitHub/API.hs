{-# LANGUAGE DuplicateRecordFields #-}

-- | GitHub API operations
--
-- This module provides GitHub API calls, mainly through their data types and endpoint names
--
module GitHub.API where

import Data.Aeson
import GHC.Generics
import GitHub.Effects (GitHubApi, post)
import Polysemy

newtype RepositoryCreateRequest = RepositoryCreateRequest {name :: String}
  deriving (Show, Generic, ToJSON)

data RepositoryCreateResponse = RepositoryCreateResponse {name :: String, owner :: String}
  deriving (Show, Generic, FromJSON)

createRepo :: (Member GitHubApi r) => String -> String -> Bool -> Sem r RepositoryCreateResponse
createRepo name _description _private = do
  post "/repo/create" RepositoryCreateRequest {name = name}