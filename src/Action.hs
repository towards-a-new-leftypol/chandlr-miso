{-# LANGUAGE ExistentialQuantification #-}

module Action where

import Data.Text (Text)
import Data.Aeson (FromJSON)
import Data.Int (Int64)
import Miso (URI)

import qualified Component.CatalogGrid as Grid
import qualified Network.ClientTypes as C
import Network.CatalogPostType (CatalogPost)
import Network.Http (HttpResult)
import Network.SiteType (Site)
import qualified Component.ThreadView as Thread

data GetThreadArgs = GetThreadArgs
    { website         :: Text
    , board_pathpart  :: Text
    , board_thread_id :: Int64
    }

data Action
    = GridAction Grid.Action
    | GetLatest
    | GetThread GetThreadArgs
    | HaveLatest (HttpResult [ CatalogPost ])
    | HaveThread (HttpResult [ Site ])
    | forall a. (FromJSON a) => ClientAction (HttpResult a -> Action) (C.Action a)
    | ThreadAction Thread.Action
    | ChangeURI URI
    | NoAction
