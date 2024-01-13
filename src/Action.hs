module Action where

import Component.CatalogGrid as Grid
import Network.Client as Client
import Data.Text (Text)

import Common.PostsType (Post)

data Action
    = GridAction Grid.Action
    | GetLatest
    | GetThread
        { website         :: String
        , board           :: String
        , board_thread_id :: Int
        }
    | HaveLatest (Client.HttpResult [Post])
    | ClientAction Client.Action
    | NoAction
