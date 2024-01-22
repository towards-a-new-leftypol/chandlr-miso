{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.CatalogPostType
    ( CatalogPost (..) )
    where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (UTCTime) -- Required for timestamp with time zone
import Data.Int (Int64)
import Data.Text (Text)

data CatalogPost = CatalogPost
    { post_id         :: Maybe Int64
    , board_post_id   :: Int64
    , board_thread_id :: Int64
    , creation_time   :: UTCTime
    , bump_time       :: UTCTime
    , body            :: Maybe Text
    , name            :: Maybe Text
    , subject         :: Maybe Text
    , email           :: Maybe Text
    , thread_id       :: Int
    -- , post_count      :: Int
    , estimated_post_count :: Int
    , site_name       :: String
    , pathpart        :: String
    --, site_id         :: Int
    } deriving (Show, Generic, FromJSON, ToJSON, Eq)
