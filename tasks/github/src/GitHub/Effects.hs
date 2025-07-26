{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module GitHub.Effects where

import Data.Kind (Type)
import Data.Aeson
import Polysemy
import Runix.Effects hiding (Post, post)
import qualified Runix.Effects as REST
import Polysemy.Fail

-- there is probably a better alternative, but urls are kina file paths?
import System.FilePath ( (</>) )

data GitHubApi (m :: Type -> Type) a where
    Post :: (ToJSON reqdata, FromJSON respdata) => String -> reqdata -> GitHubApi m respdata
post :: (Member GitHubApi r, ToJSON reqdata, FromJSON respdata) => String -> reqdata -> Sem r respdata
post p d = send $ Post p d 

newtype GitHub = GitHub {
    token :: String
}

instance RestEndpoint GitHub where
    apiroot _ = "https://api.github.com/"
    authheaders p = [("Authorization", "Bearer " <> p.token)]


runGitHubApi :: Members [RestAPI GitHub, Fail] r => Sem (GitHubApi : r) a -> Sem r a
runGitHubApi = interpret $ \case
    Post path req -> REST.post @GitHub (Endpoint $ "https://api.github.com" </> path ) req