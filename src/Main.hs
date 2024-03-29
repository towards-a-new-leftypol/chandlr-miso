{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Data.Maybe (maybe, fromJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Network.URI (uriPath, uriQuery, escapeURIString, unEscapeString, isAllowedInURI)
import System.FilePath ((</>))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Data.JSString (pack, append, unpack)
import Data.JSString.Text (textFromJSString)
import Miso
    ( View
    , miso
    , App (..)
    , Effect
    , (<#)
    , noEff
    , defaultEvents
    , LogLevel (Off)
    , URI
    , runRoute
    , consoleLog
    , pushURI
    , uriSub
    --, getCurrentURI
    )
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types (toJSString, fromJSString, Element, JSString)
import GHCJS.DOM.ParentNode (querySelector)
import GHCJS.DOM.Element (getAttribute)
import Servant.API
import Data.Aeson (decodeStrict, FromJSON)

import Common.FrontEnd.Action
import Common.FrontEnd.Routes
import qualified Network.Client as Client
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.ThreadView as Thread
import qualified Common.Component.TimeControl as TC
import qualified Common.Component.Search.SearchTypes as Search
import qualified Component.Search as Search
import Common.Network.SiteType (Site)
import Common.FrontEnd.Views
import Common.FrontEnd.Model
import Common.FrontEnd.Interfaces
import Common.Network.CatalogPostType (CatalogPost)

data InitialData
    = CatalogData [ CatalogPost ]
    | SearchData [ CatalogPost ]
    | ThreadData Site
    | Nil

parseInitialDataUsingRoute :: Model -> URI -> JSString -> InitialData
parseInitialDataUsingRoute model uri raw_json = either (const Nil) id routing_result
    where
        decoded_thing :: (FromJSON a) => Maybe a
        decoded_thing = decodeStrict $ encodeUtf8 $ textFromJSString raw_json

        routing_result = runRoute (Proxy :: Proxy Route) handlers (const uri) model

        handlers = h_latest :<|> h_thread :<|> h_search

        h_latest :: Model -> InitialData
        h_latest _ = CatalogData $ maybe ([]) id $ decoded_thing

        h_thread :: Text -> Text -> BoardThreadId -> Model -> InitialData
        h_thread _ _ _ _ = undefined

        h_search :: Maybe Text -> Model -> InitialData
        h_search _ _ = undefined

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


getMetadata :: JSString -> IO (Maybe JSString)
getMetadata key = do
    doc <- currentDocument

    mElem :: Maybe Element <- case doc of
        Nothing -> return Nothing
        Just d -> querySelector d $ "meta[name='" <> key <> "']"

    case mElem of
        Nothing -> return Nothing
        Just el -> getAttribute el ("content" :: JSString)


main :: IO ()
main = do
    consoleLog "Hello World!"

    pg_api_root <- getMetadata "postgrest-root" >>=
        return . maybe "http://localhost:3000" id
    consoleLog pg_api_root

    pg_fetch_count <- getMetadata "postgrest-fetch-count" >>=
        return . maybe 1000 (read . fromJSString)

    media_root <- getMetadata "media-root" >>=
        return . maybe "undefined" id

    now <- getCurrentTime

    -- uri <- getCurrentURI

    initial_data <- getMetadata "initial-data" >>= return . maybe "" id

    -- how to decode initial_data:
    --      - need to use runRoute but return some kind of new data type that wraps
    --      our data for each view, with constructors like InitialCatalog [ CatalogPost ] etc
    --
    --      - to use this, need to pass in a Model

    search_var <- newEmptyMVar

    miso $ \uri ->
            let initial_model = initialModel
                  pg_api_root
                  pg_fetch_count
                  media_root
                  uri
                  now
                  search_var
            in
                App
                    { model         = parseInitialData initial_model uri initial_data
                    , update        = mainUpdate
                    , view          = mainView
                    , subs          = [ uriSub ChangeURI ]
                    , events        = defaultEvents
                    , initialAction = NoAction --initialActionFromRoute initial_model uri
                    , mountPoint    = Nothing
                    , logLevel      = Off
                    }

    where
        parseInitialData :: Model -> URI -> JSString -> Model
        parseInitialData m uri json_str = applyInitialData m initial_data
            where
                applyInitialData :: Model -> InitialData -> Model
                applyInitialData model (CatalogData posts) =
                    model { grid_model = Grid.Model posts (media_root_ model) }

                initial_data = parseInitialDataUsingRoute m uri json_str

mainView :: Model -> View Action
mainView model = view
    where
        view =
          either (const page404) id
            $ runRoute (Proxy :: Proxy Route) handlers current_uri model

        handlers = catalogView :<|> threadView :<|> searchView

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
