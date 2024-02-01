module Network.ClientTypes where

import qualified Network.Http as Http
import Network.CatalogPostType (CatalogPost)
import GHCJS.DOM.Types (JSString)

data Action = Connect (Http.HttpActionResult [CatalogPost])

data Interface a = Interface
    { passAction :: Action -> a
    , returnResult :: Http.HttpResult [CatalogPost] -> a
    }

data Model = Model
  { pgApiRoot :: JSString
  , fetchCount :: Int
  } deriving Eq


