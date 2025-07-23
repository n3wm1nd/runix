{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module GitHub.Effects where

import Data.Kind (Type)
import Data.Aeson
import Polysemy
import Runix.Effects (RestAPI, restPost, Secret, getSecret, Endpoint (Endpoint))
import Polysemy.Fail

-- there is probably a better alternative, but urls are kina file paths?
import System.FilePath ( (</>) )

data GitHubApi (m :: Type -> Type) a where
    Post :: (ToJSON reqdata, FromJSON respdata) => String -> reqdata -> GitHubApi m respdata
post :: (Member GitHubApi r, ToJSON reqdata, FromJSON respdata) => String -> reqdata -> Sem r respdata
post p d = send $ Post p d

data GithubAuthentication = BearerToken String

runGitHubApi :: forall r a. (Member RestAPI r, Member Fail r) => GithubAuthentication -> Sem (GitHubApi : r) a -> Sem r a
runGitHubApi auth = interpret $ \case
    Post path req -> restPost (Endpoint $ "https://api.github.com" </> path ) req

runGitHubApiSecret :: forall r a. (Member (Secret GithubAuthentication) r,  Member RestAPI r, Member Fail r) => Sem (GitHubApi : r) a -> Sem r a
runGitHubApiSecret s1 = getSecret >>= flip runGitHubApi s1