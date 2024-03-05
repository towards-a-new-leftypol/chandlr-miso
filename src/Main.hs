{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Data.Maybe (maybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (uriPath, uriQuery, escapeURIString, unEscapeString, isAllowedInURI)
import System.FilePath ((</>))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Concurrent.MVar (MVar, newEmptyMVar)

import Data.Aeson (FromJSON)
import Data.JSString (pack, append, unpack)
import Miso
    ( View
    , startApp
    , App (..)
    , h1_
    , div_
    , text
    , Effect
    , (<#)
    , noEff
    , defaultEvents
    , LogLevel (Off)
    , URI
    , runRoute
    , getCurrentURI
    , consoleLog
    , pushURI
    , uriSub
    , time_
    , class_
    )
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types (toJSString, fromJSString, Element, JSString)
import GHCJS.DOM.ParentNode (querySelector)
import GHCJS.DOM.Element (getAttribute)
import Servant.API

import Common.FrontEnd.Action
import Common.FrontEnd.Routes
import qualified Network.Client as Client
import Network.CatalogPostType (CatalogPost)
import qualified Network.CatalogPostType as CatalogPost
import qualified Component.CatalogGrid as Grid
import qualified Component.ThreadView as Thread
import qualified Component.TimeControl as TC
import qualified Component.Search as Search
import qualified Component.Search.SearchTypes as Search


data Model = Model
    { grid_model :: Grid.Model
    , client_model :: Client.Model
    , thread_model :: Maybe Thread.Model
    , current_uri :: URI
    , media_root_ :: JSString
    , current_time :: UTCTime
    , tc_model :: TC.Model
    , search_model :: Search.Model
    {-
     - Goal:
     -  handle search results
     -      - which means displaying them in a catalog
     -      - which means having STATE that says we should be displaying
     -      search results.
     -          - so we need the results somewhere
     -          - also need to change the url to show results
     -}
    } deriving Eq


initialActionFromRoute :: Model -> URI -> Action
initialActionFromRoute model uri = either (const NoAction) id routing_result
    where
        routing_result = runRoute (Proxy :: Proxy Route) handlers (const uri) model

        handlers = h_latest :<|> h_thread :<|> h_search

        h_latest :: Model -> Action
        h_latest = const $ GoToTime $ current_time model

        h_thread :: Text -> Text -> BoardThreadId -> Model -> Action
        h_thread website board_pathpart board_thread_id _ = GetThread GetThreadArgs {..}

        h_search :: Maybe Text -> Model -> Action
        h_search Nothing m = GoToTime $ current_time m
        h_search (Just search_query) m
            | Search.searchTerm (search_model m) == unescaped_search_query = SearchResults unescaped_search_query
            | otherwise = (Search.passAction iSearch) $ Search.ChangeAndSubmit unescaped_search_query

            where
                unescaped_search_query = toJSString $ unEscapeString $ T.unpack search_query


initialModel
    :: JSString
    -> Int
    -> JSString
    -> URI
    -> UTCTime
    -> MVar JSString
    -> Model
initialModel pgroot client_fetch_count media_root u t smv = Model
    { grid_model = Grid.initialModel media_root
    , client_model = client_model_
    , thread_model = Nothing
    , current_uri = u
    , media_root_ = media_root
    , current_time = t
    , tc_model = TC.initialModel 0
    , search_model = Search.Model
        { Search.searchTerm = ""
        , Search.searchVar = smv
        , Search.clientModel = client_model_
        , Search.displayResults = []
        }
    }

    where
      client_model_ = Client.Model
        { Client.pgApiRoot = pgroot
        , Client.fetchCount = client_fetch_count
        }


getMetadata :: String -> IO (Maybe JSString)
getMetadata key = do
    doc <- currentDocument

    mElem :: Maybe Element <- case doc of
        Nothing -> return Nothing
        Just d -> querySelector d $ "meta[name='" ++ key ++ "']"

    case mElem of
        Nothing -> return Nothing
        Just el -> getAttribute el ("content" :: JSString)


main :: IO ()
main = do
    consoleLog "Hello World!"

    uri <- getCurrentURI

    consoleLog $ toJSString $ show uri

    pg_api_root <- getMetadata "postgrest-root" >>=
        return . maybe "http://localhost:2000" id
    consoleLog pg_api_root

    pg_fetch_count <- getMetadata "postgrest-fetch-count" >>=
        return . maybe 1000 (read . fromJSString)

    media_root <- getMetadata "media-root" >>=
        return . maybe "undefined" id

    now <- getCurrentTime

    search_var <- newEmptyMVar

    let initial_model = initialModel
          pg_api_root
          pg_fetch_count
          media_root uri
          now
          search_var

    startApp App
        { model         = initial_model
        , update        = mainUpdate
        , view          = mainView
        , subs          = [ uriSub ChangeURI ]
        , events        = defaultEvents
        , initialAction = initialActionFromRoute initial_model uri
        , mountPoint    = Nothing
        , logLevel      = Off
        }


mainView :: Model -> View Action
mainView model = view
    where
        view =
          either (const page404) id
            $ runRoute (Proxy :: Proxy Route) handlers current_uri model

        handlers = catalog_view :<|> thread_view :<|> search_view

        catalog_view :: Model -> View Action
        catalog_view m = div_ []
            [ div_
                [ class_ "page_heading" ]
                [ h1_ [] [ text "Overboard Catalog" ]
                , time_ [] [ text $ pack $ show $ current_time model ]
                ]
            , TC.view iTime (tc_model m)
            , Search.view iSearch (search_model m)
            , Grid.view iGrid (grid_model model)
            ]

        thread_view :: Text -> Text -> BoardThreadId -> Model -> View Action
        thread_view site_name board_pathpart board_thread_id m = maybe
            (h1_ [] [ text "Thread View" ])
            Thread.view
            (thread_model m)

        search_view :: Maybe Text -> Model -> View Action
        search_view _ m = div_ []
            [ div_
                [ class_ "page_heading" ]
                [ h1_ [] [ text "Search" ]
                , time_ [] [ text $ Search.searchTerm $ search_model m ]
                ]
            , Search.view iSearch (search_model m)
            , Grid.view iGrid $ (grid_model model)
                    { Grid.display_items = (Search.displayResults (search_model m))
                    }
            ]

        page404 :: View Action
        page404 = h1_ [] [ text "404 Not Found" ]


mainUpdate :: Action -> Model -> Effect Action Model
mainUpdate NoAction m = noEff m
mainUpdate (HaveLatest Client.Error) m = m <# do
    consoleLog "Getting Latest failed!"
    return NoAction

mainUpdate (HaveLatest (Client.HttpResponse {..})) m = m <#
    case body of
        Nothing -> do
            consoleLog "Didn't get anything back from API"
            return NoAction
        Just posts -> do
            -- mapM_ (consoleLog . toJSString . show) posts
            return $ GridAction $ Grid.DisplayItems posts

mainUpdate (HaveThread Client.Error) m = m <# do
    consoleLog "Getting Thread failed!"
    return NoAction

mainUpdate (HaveThread (Client.HttpResponse {..})) m = new_model <# do
    consoleLog "Have Thread!"
    return $ ThreadAction $ Thread.RenderSite $ Thread.site $ fromJust $ thread_model new_model

    where
        new_model = m
            { thread_model =
                body >>= Just . (Thread.initialModel $ media_root_ m) . head
            }

mainUpdate (GoToTime t) m = m { current_time = t } <# do
  Client.fetchLatest (client_model m) t (iClient HaveLatest)

mainUpdate (GetThread GetThreadArgs {..}) m = m <# do
    consoleLog $ "Thread " `append` (pack $ show $ board_thread_id)
    pushURI new_current_uri
    Client.getThread (client_model m) (iClient HaveThread) GetThreadArgs {..}

    where
        new_current_uri :: URI
        new_current_uri = (current_uri m)
            { uriPath = T.unpack website
                    </> T.unpack board_pathpart
                    </> show board_thread_id
            , uriQuery = ""
            }

mainUpdate (ChangeURI uri) m = m { current_uri = uri } <# do
    consoleLog $ "ChangeURI! " `append` (pack $ show $ uri)
    return NoAction

mainUpdate (GridAction ga) m =
    Grid.update iGrid ga (grid_model m)
    >>= \gm -> noEff (m { grid_model = gm })

mainUpdate (ClientAction action ca) m =
    Client.update (iClient action) ca (client_model m)
    >>= \cm -> noEff (m { client_model = cm })

mainUpdate (ThreadAction ta) model = do
    tm :: Maybe Thread.Model <- case thread_model model of
        Nothing -> noEff Nothing
        Just m -> Thread.update iThread ta m >>= return . Just

    noEff model { thread_model = tm }

mainUpdate (TimeAction ta) m =
  TC.update iTime ta (tc_model m)
  >>= \tm -> noEff m { tc_model = tm }

mainUpdate (SearchAction sa) m =
  Search.update iSearch sa (search_model m)
  >>= \sm -> noEff m { search_model = sm }

mainUpdate (SearchResults query) m = m { current_uri = new_current_uri } <# do
    consoleLog $ "SearchResults new uri: " <> (pack $ show new_current_uri)
    pushURI new_current_uri
    return NoAction

    where
        new_current_uri :: URI
        new_current_uri = (current_uri m)
            { uriPath = "/search"
            , uriQuery = "?search=" ++ (escapeURIString isAllowedInURI $ unpack query)
            }

iGrid :: Grid.Interface Action
iGrid = Grid.Interface
    { Grid.passAction = GridAction
    , Grid.threadSelected = mkGetThread
    }

    where
        mkGetThread :: CatalogPost -> Action
        mkGetThread post = GetThread GetThreadArgs
            { website = CatalogPost.site_name post
            , board_pathpart = CatalogPost.pathpart post
            , board_thread_id = CatalogPost.board_thread_id post
            }

iClient :: (FromJSON a) => (Client.HttpResult a -> Action) -> Client.Interface Action a
iClient action = Client.Interface
    { Client.passAction = ClientAction action
    , Client.returnResult = action
    }

iThread :: Thread.Interface Action
iThread = Thread.Interface { Thread.passAction = ThreadAction }

iTime :: TC.Interface Action
iTime = TC.Interface
  { TC.passAction = TimeAction
  , TC.goTo = GoToTime
  }

iSearch :: Search.Interface Action
iSearch =
  Search.Interface
    { passAction = SearchAction
    , clientIface = iClient $ SearchAction . Search.SearchResult
    , searchResults = SearchResults
    }
