{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.SiteType where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (FromJSON, ToJSON)
import Network.BoardType (Board)

data Site = Site
  { site_id :: Int
  , name    :: Text
  , url     :: Text
  , boards  :: [ Board ]
  } deriving (Show, Generic, FromJSON, ToJSON, Eq)
