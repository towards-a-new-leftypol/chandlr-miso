module Action where

import Component.CatalogGrid as Grid

data Action
    = GridAction Grid.Action
    | GetLatest
    | GetThread
        { website         :: String
        , board           :: String
        , board_thread_id :: Int
        }
    | NoAction
