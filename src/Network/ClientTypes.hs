module Network.ClientTypes where

import qualified Network.Http as Http
import Network.CatalogPostType (CatalogPost)
import GHCJS.DOM.Types (JSString)

data Action a = Connect (Http.HttpActionResult a)

data Interface a b = Interface
    { passAction :: Action b -> a
    , returnResult :: Http.HttpResult b -> a
    }

data Model = Model
  { pgApiRoot :: JSString
  , fetchCount :: Int
  } deriving Eq


