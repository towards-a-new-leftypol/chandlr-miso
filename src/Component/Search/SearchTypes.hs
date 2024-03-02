module Component.Search.SearchTypes where

import Data.JSString (JSString)
import Network.Http (HttpResult (..))

import qualified Network.ClientTypes as Client
import Network.CatalogPostType (CatalogPost)

data Action
    = SearchChange JSString
    | OnSubmit
    | SearchResult (HttpResult [ CatalogPost ])
    | PassPostsToSelf [ CatalogPost ] -- I think I don't understand something about the update type but I had to add this...
    | NoAction

data Model = Model
    { searchTerm :: JSString
    , clientModel :: Client.Model
    , displayResults :: [ CatalogPost ]
    } deriving Eq

data Interface a = Interface
    { passAction :: Action -> a
    , clientIface :: Client.Interface a [ CatalogPost ]
    , searchResults :: a
    }
