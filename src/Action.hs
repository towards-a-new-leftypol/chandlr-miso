module Action where

import Data.Text (Text)
import Component.CatalogGrid as Grid
import Data.Int (Int64)
import Miso (URI)

import Network.ClientTypes as C
import Network.CatalogPostType (CatalogPost)
import Network.Http (HttpResult)

data GetThreadArgs = GetThreadArgs
    { website         :: Text
    , board_pathpart  :: Text
    , board_thread_id :: Int64
    }

data Action
    = GridAction Grid.Action
    | GetLatest
    | GetThread GetThreadArgs
    | HaveLatest (HttpResult [CatalogPost])
    | ClientAction C.Action
    | ChangeURI URI
    | NoAction
