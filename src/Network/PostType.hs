{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.PostType where

import GHC.Generics
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Aeson (FromJSON, ToJSON)
import Common.AttachmentType (Attachment)

data Post = Post
  { post_id           :: Integer
  , board_post_id     :: Integer
  , creation_time     :: UTCTime
  , body              :: Maybe Text
  , subject           :: Maybe Text
  , name              :: Maybe Text
  , email             :: Maybe Text
  , body_search_index :: Text
  , thread_id         :: Integer
  , attachments       :: [ Attachment ]
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)

