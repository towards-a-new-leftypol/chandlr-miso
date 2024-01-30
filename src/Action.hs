module Action where

import Data.Text (Text)
import Component.CatalogGrid as Grid
import Network.Client as Client
import Data.Int (Int64)
import Miso (URI)

import Network.CatalogPostType (CatalogPost)

data Action
    = GridAction Grid.Action
    | GetLatest
    | GetThread
        { website         :: Text
        , board_pathpart  :: Text
        , board_thread_id :: Int64
        }
    | HaveLatest (Client.HttpResult [CatalogPost])
    | ClientAction Client.Action
    | ChangeURI URI
    | NoAction
