module Component.Thread.Model where

import GHCJS.DOM.Types (JSString)
import Common.Network.SiteType (Site)
import Common.Network.PostType (Post)
import Parsing.PostPartType (PostPart)
import Data.Time.Clock (UTCTime)

type PostWithBody = (Post, [ PostPart ])

data Model = Model
  { site :: Site
  , media_root :: JSString
  , post_bodies :: [ PostWithBody ]
  , current_time :: UTCTime
  } deriving Eq

