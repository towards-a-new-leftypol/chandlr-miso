module Component.Thread.Model where

import GHCJS.DOM.Types (JSString)
import Network.SiteType (Site)
import Network.PostType (Post)
import BodyParser (PostPart)

type PostWithBody = (Post, [ PostPart ])

data Model = Model
  { site :: Site
  , media_root :: JSString
  , post_bodies :: [ PostWithBody ]
  } deriving Eq

