module Action where

import Component.CatalogGrid as Grid
import Network.Client as Client

import Network.CatalogPostType (CatalogPost)

data Action
    = GridAction Grid.Action
    | GetLatest
    | GetThread
        { website         :: String
        , board           :: String
        , board_thread_id :: Int
        }
    | HaveLatest (Client.HttpResult [CatalogPost])
    | ClientAction Client.Action
    | NoAction
