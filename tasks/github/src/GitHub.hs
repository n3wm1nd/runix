{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | GitHub domain operations and task re-exports
--
-- This module provides GitHub API operations, Git workflow tasks,
-- and the Git effect for building custom GitHub integrations.
--
-- Typical usage:
--
-- @
-- import qualified GitHub
--
-- -- Domain operations
-- repos <- GitHub.listRepos "myorg"
-- issues <- GitHub.getIssues repo
-- @
module GitHub
  ( -- * Types
    Repository,
    RepositoryName,
    createRepo,
  )
where


import qualified GitHub.API as API
import GitHub.Effects (GitHubApi)
import Polysemy

-- Domain operations (GitHub API calls)
-- These use RestAPI effect to make actual GitHub API calls

newtype GithubUser = GithubUser String
newtype RepositoryName = RepositoryName String
newtype RepositoryDescription = RepositoryDescription String
data RepositoryVisibility = Private | Public
isprivate :: RepositoryVisibility -> Bool
isprivate Private = True
isprivate Public = False

data Repository = Repository {name :: RepositoryName, owner :: GithubUser}

-- Create a new repository on GitHub 
createRepo :: (Members '[GitHubApi] r) => 
  RepositoryName -> RepositoryDescription -> RepositoryVisibility -> Sem r Repository
createRepo (RepositoryName name) (RepositoryDescription description) visibility = do
  repo <- API.createRepo name description (isprivate visibility)
  return  Repository {name = RepositoryName repo.name, owner = GithubUser repo.owner}