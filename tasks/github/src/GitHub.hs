{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

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
    GitHub(..),
    createRepo,
  )
where


import qualified GitHub.API as API
import Polysemy
import Runix.Effects 

newtype GitHub = GitHub {
    token :: String
}

instance RestEndpoint GitHub where
    apiroot _ = "https://api.github.com/"
    authheaders p = [("Authorization", "Bearer " <> p.token)]

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
createRepo :: Members '[RestAPI GitHub] r => 
  RepositoryName -> RepositoryDescription -> RepositoryVisibility -> Sem r Repository
createRepo (RepositoryName name) (RepositoryDescription description) visibility = do
  let req = API.RepositoryCreateRequest 
              { API.name = name
              , API.description = Just description
              , API.homepage = Nothing
              , API.private = Just (isprivate visibility)
              }
  (repo :: API.RepositoryCreateResponse) <- post @GitHub (Endpoint "/user/repos") req
  return Repository {name = RepositoryName repo.name, owner = GithubUser (API.login $ API.owner repo)}

-- List all repositories for the authenticated user
-- listRepos :: Members '[RestAPI GitHub] r => Sem r [Repository]

-- Get details of a specific repository
-- getRepo :: Members '[RestAPI GitHub] r => RepositoryName -> Sem r Repository

-- Delete a repository
-- deleteRepo :: Members '[RestAPI GitHub] r => RepositoryName -> Sem r ()

-- Create or update a file in a repository
-- createFile :: Members '[RestAPI GitHub] r => RepositoryName -> FilePath -> String -> Sem r ()

-- Read content of a file from a repository
-- readFile :: Members '[RestAPI GitHub] r => RepositoryName -> FilePath -> Sem r String

-- Create a new issue
-- createIssue :: Members '[RestAPI GitHub] r => RepositoryName -> String -> String -> Sem r ()

-- List issues for a repository
-- listIssues :: Members '[RestAPI GitHub] r => RepositoryName -> Sem r [Issue]
