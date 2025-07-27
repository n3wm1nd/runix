{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | GitHub API operations
--
-- This module provides GitHub API calls, mainly through their data types and endpoint names
--
module GitHub.API
  ( RepositoryCreateRequest(..)
  , Owner(..)
  , RepositoryCreateResponse(..)
  ) where

import Data.Aeson
import GHC.Generics

data RepositoryCreateRequest = RepositoryCreateRequest 
  { name :: String
  , description :: Maybe String
  , homepage :: Maybe String
  , private :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON RepositoryCreateRequest

data Owner = Owner
  { login :: String
  , id :: Int
  , nodeId :: String
  , avatarUrl :: String
  , gravatarId :: Maybe String
  , url :: String
  , htmlUrl :: String
  , followersUrl :: String
  , followingUrl :: String
  , gistsUrl :: String
  , starredUrl :: String
  , subscriptionsUrl :: String
  , organizationsUrl :: String
  , reposUrl :: String
  , eventsUrl :: String
  , receivedEventsUrl :: String
  , type_ :: String
  , siteAdmin :: Bool
  } deriving (Show, Generic)

data RepositoryCreateResponse = RepositoryCreateResponse 
  { name :: String
  , owner :: Owner
  , private :: Bool
  , htmlUrl :: String
  , description :: Maybe String
  , fork :: Bool
  , url :: String
  , createdAt :: String
  , updatedAt :: String
  , pushedAt :: String
  , gitUrl :: String
  , sshUrl :: String
  , cloneUrl :: String
  , svnUrl :: String
  , homepage :: Maybe String
  , size :: Int
  , stargazersCount :: Int
  , watchersCount :: Int
  , language :: Maybe String
  , hasIssues :: Bool
  , hasProjects :: Bool
  , hasDownloads :: Bool
  , hasWiki :: Bool
  , hasPages :: Bool
  , forksCount :: Int
  , mirrorUrl :: Maybe String
  , archived :: Bool
  , disabled :: Bool
  , openIssuesCount :: Int
  , license :: Maybe String
  , allowForking :: Maybe Bool
  , isTemplate :: Maybe Bool
  , webCommitSignoffRequired :: Maybe Bool
  , topics :: Maybe [String]
  , defaultBranch :: String
  , id :: Int
  , nodeId :: String
  } deriving (Show, Generic)

-- Custom field name mappings for JSON parsing
instance FromJSON Owner where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \case
        "type_" -> "type"
        "nodeId" -> "node_id"
        "avatarUrl" -> "avatar_url"
        "gravatarId" -> "gravatar_id"
        "htmlUrl" -> "html_url"
        "followersUrl" -> "followers_url"
        "followingUrl" -> "following_url"
        "gistsUrl" -> "gists_url"
        "starredUrl" -> "starred_url"
        "subscriptionsUrl" -> "subscriptions_url"
        "organizationsUrl" -> "organizations_url"
        "reposUrl" -> "repos_url"
        "eventsUrl" -> "events_url"
        "receivedEventsUrl" -> "received_events_url"
        "siteAdmin" -> "site_admin"
        fieldName -> fieldName
    }

instance FromJSON RepositoryCreateResponse where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \case
        "nodeId" -> "node_id"
        "htmlUrl" -> "html_url"
        "gitUrl" -> "git_url"
        "sshUrl" -> "ssh_url"
        "cloneUrl" -> "clone_url"
        "svnUrl" -> "svn_url"
        "hasIssues" -> "has_issues"
        "hasProjects" -> "has_projects"
        "hasDownloads" -> "has_downloads"
        "hasWiki" -> "has_wiki"
        "hasPages" -> "has_pages"
        "forksCount" -> "forks_count"
        "mirrorUrl" -> "mirror_url"
        "openIssuesCount" -> "open_issues_count"
        "allowForking" -> "allow_forking"
        "isTemplate" -> "is_template"
        "webCommitSignoffRequired" -> "web_commit_signoff_required"
        "defaultBranch" -> "default_branch"
        "createdAt" -> "created_at"
        "updatedAt" -> "updated_at"
        "pushedAt" -> "pushed_at"
        fieldName -> fieldName
    }
