{-# LANGUAGE ExistentialQuantification #-}

module Action where

import Data.Text (Text)
import Data.Aeson (FromJSON)
import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Miso (URI)

import qualified Component.CatalogGrid as Grid
import qualified Network.ClientTypes as C
import Network.CatalogPostType (CatalogPost)
import Network.Http (HttpResult)
import Network.SiteType (Site)
import qualified Component.ThreadView as Thread
import qualified Component.TimeControl as TC
import qualified Component.Search.SearchTypes as Search

data GetThreadArgs = GetThreadArgs
    { website         :: Text
    , board_pathpart  :: Text
    , board_thread_id :: Int64
    }

data Action
    = GridAction Grid.Action
    | GetThread GetThreadArgs
    | HaveLatest (HttpResult [ CatalogPost ])
    | HaveThread (HttpResult [ Site ])
    | forall a. (FromJSON a) => ClientAction (HttpResult a -> Action) (C.Action a)
    | ThreadAction Thread.Action
    | TimeAction TC.Time
    | SearchAction Search.Action
    | GoToTime UTCTime
    | ChangeURI URI
    | NoAction
