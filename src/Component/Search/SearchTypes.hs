module Component.Search.SearchTypes where

import Data.JSString (JSString)
import Network.Http (HttpResult (..))

import qualified Network.ClientTypes as Client
import Network.CatalogPostType (CatalogPost)

data Action
    = SearchChange JSString
    | OnSubmit
    | SearchResult (HttpResult [ CatalogPost ])
    | DisplayResults [ CatalogPost ]
    | NoAction

data Model = Model
    { searchTerm :: JSString
    , clientModel :: Client.Model
    } deriving Eq

data Interface a = Interface
    { passAction :: Action -> a
    , clientIface :: Client.Interface a [ CatalogPost ]
    , searchResults :: [ CatalogPost ] -> a
    }
