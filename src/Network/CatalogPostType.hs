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
-- import Common.AttachmentType (Dimension)

data CatalogPost = CatalogPost
    { post_id              :: Maybe Int64
    , board_post_id        :: Int64
    , board_thread_id      :: Int64
    , creation_time        :: UTCTime
    , bump_time            :: UTCTime
    , body                 :: Maybe Text
    , name                 :: Maybe Text
    , subject              :: Maybe Text
    , email                :: Maybe Text
    , thread_id            :: Int
    -- , post_count           :: Int
    , estimated_post_count :: Int
    , site_name            :: String
    , pathpart             :: String
    -- , site_id              :: Int
    , file_mimetype        :: Maybe Text
    , file_illegal         :: Maybe Bool
    -- , file_resolution      :: Maybe Dimension
    , file_sha256_hash     :: Maybe Text
    , file_extension       :: Maybe Text
    } deriving (Show, Generic, FromJSON, ToJSON, Eq)
