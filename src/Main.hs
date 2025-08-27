{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.URI (escapeURIString, unEscapeString, isAllowedInURI)
import System.FilePath ((</>))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Data.JSString (JSString)
import Miso
    ( View (..)
    , Effect
    , defaultEvents
    , LogLevel (DebugAll)
    , route
    , consoleLog
    , consoleError
    , pushURI
    , uriSub
    , run
    , miso
    , Component (..)
    , getURI
    , modify
    , io_
    , get
    , put
    , issue
    , getComponentId
    , publish
    , subscribe
    )
import Miso.String (MisoString, toMisoString, fromMisoString)
import Servant.API
import Data.Aeson (decodeStrict, FromJSON)
import Control.Monad.IO.Class (liftIO)
import Language.Javascript.JSaddle.Monad (JSM)

import Common.FrontEnd.Action
import Common.FrontEnd.Routes
import qualified Network.Client as Client
import qualified Common.Network.ClientTypes as Client
import qualified Common.Component.Thread as Thread
import qualified Common.Component.TimeControl as TC
import qualified Common.Component.Search.SearchTypes as Search
import qualified Common.Component.CatalogGrid.GridTypes as Grid
import qualified Common.Component.CatalogGrid as Grid
import Common.FrontEnd.MainComponent (MainComponent)
import Common.Network.SiteType (Site, fromCatalogPost)
import Common.FrontEnd.Views
import Common.FrontEnd.Model
import Common.Network.CatalogPostType (CatalogPost)
import JSFFI.Saddle
    ( getDocument
    , Element (..)
    , Document (..)
    , ParentNode (..)
    , querySelector
    , getAttribute
    , textContent
    )
import qualified Common.Utils as Utils

data InitialData
    = CatalogData [ CatalogPost ]
    | SearchData [ CatalogPost ]
    | ThreadData Site [ Thread.PostWithBody ]
    | Nil

pattern Sender :: Client.Sender
pattern Sender = "main"

pattern SenderLatest :: Client.Sender
pattern SenderLatest = "main-latest"

pattern SenderThread :: Client.Sender
pattern SenderThread = "main-thread"

parseInitialDataUsingRoute :: Model -> URI -> MisoString -> InitialData
parseInitialDataUsingRoute model uri raw_json = either (const Nil) id routing_result
    where
        decoded_thing :: (FromJSON a) => Maybe a
        decoded_thing = decodeStrict $ encodeUtf8 $ fromMisoString raw_json

        routing_result = route (Proxy :: Proxy (Route (View Action))) handlers (const uri) model

        handlers = h_latest :<|> h_thread :<|> h_search

        h_latest :: Model -> InitialData
        h_latest _ = CatalogData $ maybe ([]) id $ decoded_thing

        h_thread :: Text -> Text -> BoardThreadId -> Model -> InitialData
        h_thread _ _ _ _ = maybe Nil (flip ThreadData $ undefined) $ decoded_thing

        h_search :: Maybe String -> Model -> InitialData
        h_search _ _ = undefined

initialActionFromRoute :: Model -> URI -> Action
initialActionFromRoute model uri = either (const NoAction) id routing_result
    where
        routing_result = route (Proxy :: Proxy (Route (View Action))) handlers (const uri) model

        handlers = h_latest :<|> h_thread :<|> h_search

        h_latest :: Model -> Action
        h_latest = const $ GoToTime $ current_time model

        h_thread :: Text -> Text -> BoardThreadId -> Model -> Action
        h_thread website board_pathpart board_thread_id _ =
            GetThread Client.GetThreadArgs
                { Client.website = toMisoString website
                , Client.board_pathpart = toMisoString board_pathpart
                , Client.board_thread_id = board_thread_id
                }

        h_search :: Maybe String -> Model -> Action
        h_search Nothing m = GoToTime $ current_time m
        h_search (Just search_query) m
            | search_term m == unescaped_search_query = SearchResults unescaped_search_query
            | otherwise = NotifySearch $ unescaped_search_query

            where
                unescaped_search_query = toMisoString $ unEscapeString $ search_query


initialModel
    :: MisoString
    -> Int
    -> MisoString
    -> URI
    -> UTCTime
    -> MVar MisoString
    -> Model
initialModel pgroot fetch_count media_root u t smv = Model
    { current_uri = u
    , media_root_ = media_root
    , current_time = t
    , search_term = ""
    , initial_action = Initialize
    , thread_message = Nothing
    , pg_api_root = pgroot
    , client_fetch_count = fetch_count
    , my_component_id = 0
    }


getMetadata :: MisoString -> JSM (Maybe MisoString)
getMetadata key = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "meta[name='" <> (fromMisoString key) <> "']"

    case mElem of
        Nothing -> return Nothing
        Just (Element el) ->
            (toMisoString <$>) <$> getAttribute el ("content" :: JSString)


getScriptContents :: MisoString -> JSM (Maybe MisoString)
getScriptContents className = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "." <> (fromMisoString className)

    case mElem of
        Nothing -> return Nothing
        Just e -> (toMisoString <$>) <$> textContent e


#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run mainMain

mainMain :: JSM ()
mainMain = do
    consoleLog "Haskell begin."

    pg_api_root <- getMetadata "postgrest-root" >>=
        return . maybe "http://localhost:3000" id
    consoleLog $ "pg_api_root: " <> pg_api_root

    pg_fetch_count <- getMetadata "postgrest-fetch-count" >>=
        return . maybe 1000 fromMisoString

    media_root <- getMetadata "media-root" >>=
        return . maybe "undefined" id

    consoleLog $ "media_root: " <> media_root

    now <- liftIO getCurrentTime

    uri <- getURI

    raw_initial_data <- getScriptContents "initial-data" >>= return . maybe "" id

    -- how to decode initial_data:
    --      - need to use runRoute but return some kind of new data type that wraps
    --      our data for each view, with constructors like InitialCatalog [ CatalogPost ] etc
    --
    --      - to use this, need to pass in a Model

    search_var <- liftIO newEmptyMVar

    let initial_model =
            ( initialModel
                pg_api_root
                pg_fetch_count
                media_root
                uri
                now
                search_var
            ) { initial_action = initialActionFromRoute initial_model uri }

    let some_initial_data = parseInitialDataUsingRoute initial_model uri raw_initial_data

    initial_data <-
            case some_initial_data of
                (ThreadData site _) ->
                    liftIO $ Thread.getPostWithBodies site
                    >>= return . ThreadData site
                x -> return x

    let app :: MainComponent = Component
            { model         = initial_model
            , update        = mainUpdate
            , view          = mainView initial_data
            , subs          = [ uriSub ChangeURI ]
            , events        = defaultEvents
            , styles = []
            , initialAction = Just Initialize
            , mountPoint    = Nothing
            , logLevel      = DebugAll
            , scripts = []
            , mailbox = const Nothing
            }

    miso $ const app


mainView :: InitialData -> Model -> View Action
mainView initial_data model = view
    where
        view = either (const page404) id $
            route (Proxy :: Proxy (Route (View Action))) handlers current_uri model

        handlers
            =    (catalogView tc (grid initial_data))
            :<|> (threadView $ thread_model initial_data)
            :<|> (searchView (grid initial_data))

        tc :: TC.TimeControl
        tc = TC.app 0

        thread_model :: InitialData -> Thread.Model
        thread_model (ThreadData site posts_w_bodies) =
            Thread.Model
                { Thread.site = site
                , Thread.media_root = media_root_ model
                , Thread.post_bodies = posts_w_bodies
                , Thread.current_time = current_time model
                }
        thread_model _ = error "Not Thread Data"

        grid :: InitialData -> Grid.GridComponent
        grid initial_data_ = Grid.app initial_model
            where
                initial_model = Grid.Model
                    { Grid.display_items = initialItems initial_data_
                    , media_root = media_root_ model
                    }

                initialItems :: InitialData -> [ CatalogPost ]
                initialItems (CatalogData catalog_posts) = catalog_posts
                initialItems (SearchData catalog_posts) = catalog_posts
                initialItems _ = error "Not Catalog Data"


mainUpdate :: Action -> Effect Model Action
mainUpdate NoAction = return ()
mainUpdate Initialize = do
    getComponentId HaveOwnComponentId
    subscribe Client.clientOutTopic ClientResponse OnErrorMessage
    subscribe Grid.catalogOutTopic GridMessage OnErrorMessage

mainUpdate (HaveOwnComponentId component_id) = 
    modify (\m -> m { my_component_id = component_id })

mainUpdate ClientMounted = do
    model <- get

    io_ $ consoleLog "Http Client Mounted!"

    publish
        Client.clientInTopic
        ( Sender
        , Client.InitModel $
            Client.Model
                (pg_api_root model)
                (client_fetch_count model)
        )

    issue $ initial_action model
    modify $ \m -> m { initial_action = NoAction }

mainUpdate ClientUnmounted = io_ $ consoleLog "Http Client Unmounted!"

mainUpdate ThreadViewMounted = do
    io_ $ consoleLog "ThreadViewMounted"

    model <- get

    maybe
        (io_ $ consoleLog "No thread_message available for sending in Main Model")
        (publish Thread.threadTopic)
        (thread_message model)

mainUpdate (GridMessage (Grid.SelectThread catalog_post)) = do
    modify
        ( \m -> m
            { thread_message = Just $
                Thread.RenderSite (media_root_ m) (fromCatalogPost catalog_post)
            }
        )

    issue $ GetThread $ mkGetThread catalog_post

mainUpdate (OnErrorMessage msg) =
    io_ $ consoleError ("Main Component OnErrorMessage decode failure: " <> toMisoString msg)

mainUpdate (ClientResponse (Client.ReturnResult SenderLatest result)) =
    Utils.helper result $ \catalog_posts ->
        publish Grid.catalogInTopic $ Grid.DisplayItems catalog_posts

mainUpdate (ClientResponse (Client.ReturnResult SenderThread result)) =
    do
        io_ $ consoleLog $ SenderThread <> " - Has result. Storing result in model."

        Utils.helper result $ \sites ->
            modify
                ( \m -> m
                    { thread_message = Just $
                        Thread.RenderSite (media_root_ m) (head sites)
                    }
                )

        issue ThreadViewMounted

mainUpdate (ClientResponse (Client.ReturnResult _ _)) = return ()

mainUpdate (GoToTime t) = do
    modify (\m -> m { current_time = t })
    publish Client.clientInTopic (SenderLatest, Client.FetchLatest t)

mainUpdate (GetThread Client.GetThreadArgs {..}) = do
    io_ $ consoleLog $ "Thread " <> (toMisoString $ show board_thread_id)

    model <- get

    io_ $ pushURI $ new_current_uri model

    publish Client.clientInTopic (SenderThread, Client.GetThread Client.GetThreadArgs {..})

    where
        new_current_uri :: Model -> URI
        new_current_uri m = (current_uri m)
            { uriPath = fromMisoString website
                    </> fromMisoString board_pathpart
                    </> show board_thread_id
            , uriQuery = ""
            }

mainUpdate (ChangeURI uri) = do
    modify (\m -> m { current_uri = uri })
    io_ $ consoleLog $ "ChangeURI! " <> (toMisoString $ show uri)


mainUpdate (SearchResults query) = do
    model <- get

    let new_uri :: URI = new_current_uri model

    io_ $ do
        consoleLog $ "SearchResults new uri: " <> (toMisoString $ show new_uri)
        pushURI new_uri

    put model { current_uri = new_uri }

    where
        new_current_uri :: Model -> URI
        new_current_uri m = (current_uri m)
            { uriPath = "/search"
            , uriQuery = "?search=" ++ (escapeURIString isAllowedInURI $ fromMisoString query)
            }

mainUpdate (NotifySearch query) = publish Search.searchTopic query
