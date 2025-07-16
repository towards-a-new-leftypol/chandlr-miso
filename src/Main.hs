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
import qualified Data.Text as T
import Network.URI (escapeURIString, unEscapeString, isAllowedInURI)
import System.FilePath ((</>))
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Data.JSString (pack, append, unpack)
import Data.JSString.Text (textFromJSString)
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
    , startComponent
    , Component (..)
    , getURI
    , modify
    , io_
    , get
    , put
    , component_
    , onMountedWith
    , issue
    , getComponentId
    , publish
    , subscribe
    )
import Miso.String (MisoString, toMisoString)
import Servant.API
import Data.Aeson (decodeStrict, FromJSON, Result(..))
import Control.Monad.IO.Class (liftIO)
import Language.Javascript.JSaddle (toJSString)
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
import Common.Network.SiteType (Site)
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
    )

data InitialData
    = CatalogData [ CatalogPost ]
    | SearchData [ CatalogPost ]
    | ThreadData Site
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
        decoded_thing = decodeStrict $ encodeUtf8 $ textFromJSString raw_json

        routing_result = route (Proxy :: Proxy Route) handlers (const uri) model

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
        routing_result = route (Proxy :: Proxy Route) handlers (const uri) model

        handlers = h_latest :<|> h_thread :<|> h_search

        h_latest :: Model -> Action
        h_latest = const $ GoToTime $ current_time model

        h_thread :: Text -> Text -> BoardThreadId -> Model -> Action
        h_thread website board_pathpart board_thread_id _ = GetThread Client.GetThreadArgs {..}

        h_search :: Maybe Text -> Model -> Action
        h_search Nothing m = GoToTime $ current_time m
        h_search (Just search_query) m
            | search_term m == unescaped_search_query = SearchResults unescaped_search_query
            | otherwise = NotifySearch $ unescaped_search_query

            where
                unescaped_search_query = toMisoString $ unEscapeString $ T.unpack search_query


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

    mElem :: Maybe Element <- querySelector doc $ "meta[name='" <> key <> "']"

    case mElem of
        Nothing -> return Nothing
        Just (Element el) ->
            (toJSString <$>) <$> getAttribute el ("content" :: MisoString)


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
        return . maybe 1000 (read . unpack)

    media_root <- getMetadata "media-root" >>=
        return . maybe "undefined" id

    consoleLog $ "media_root: " <> media_root

    now <- liftIO getCurrentTime

    uri <- getURI

    initial_data <- getMetadata "initial-data" >>= return . maybe "" id

    -- how to decode initial_data:
    --      - need to use runRoute but return some kind of new data type that wraps
    --      our data for each view, with constructors like InitialCatalog [ CatalogPost ] etc
    --
    --      - to use this, need to pass in a Model

    search_var <- liftIO newEmptyMVar

    let initial_model = initialModel
          pg_api_root
          pg_fetch_count
          media_root
          uri
          now
          search_var

    let app :: MainComponent = Component
            { model         = initial_model { initial_action = initialActionFromRoute initial_model uri }
            , update        = mainUpdate
            , view          = mainView
            , subs          = [ uriSub ChangeURI ]
            , events        = defaultEvents
            , styles = []
            , initialAction = Just Initialize
            , mountPoint    = Nothing
            , logLevel      = DebugAll
            , scripts = []
            , mailbox = const Nothing
            }

    startComponent app


addToView :: View action -> View action -> View action
addToView child (VNode a b cs ds) = VNode a b cs (child : ds)
addToView _ v = v


mainView :: Model -> View Action
mainView model = view
    where
        view = either (const page404) addClient $
            route (Proxy :: Proxy Route) handlers current_uri model

        addClient :: View Action -> View Action
        addClient = addToView $
            component_
                [ onMountedWith (const ClientMounted) ]
                Client.app

        handlers
            =    (catalogView tc grid)
            :<|> (threadView Thread.app)
            :<|> (searchView grid)

        tc :: TC.TimeControl
        tc = TC.app 0

        grid :: Grid.GridComponent
        grid = Grid.app (media_root_ model)


mainUpdate :: Action -> Effect Model Action
mainUpdate NoAction = return ()
mainUpdate Initialize = do
    getComponentId HaveOwnComponentId
    subscribe Client.clientOutTopic ClientResponse
    subscribe Grid.catalogOutTopic GridMessage

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

mainUpdate ThreadViewMounted = do
    io_ $ consoleLog "ThreadViewMounted"

    model <- get

    maybe
        (return ())
        (publish Thread.threadTopic)
        (thread_message model)

mainUpdate (GridMessage (Success (Grid.GetThread getThreadArgs))) =
    issue $ GetThread getThreadArgs

mainUpdate (GridMessage (Error msg)) =
    io_ $ consoleError ("Main Component GridMessage decode failure: " <> toMisoString msg)

mainUpdate (ClientResponse (Success (Client.ReturnResult SenderLatest result))) =
    Client.helper result $ \catalog_posts ->
        publish Grid.catalogInTopic $ Grid.DisplayItems catalog_posts

mainUpdate (ClientResponse (Success (Client.ReturnResult SenderThread result))) =
    Client.helper result $ \sites ->
        modify
            ( \m -> m
                { thread_message = Just $
                    Thread.RenderSite (media_root_ m) (head sites)
                }
            )

mainUpdate (ClientResponse (Success (Client.ReturnResult _ _))) = return ()
mainUpdate (ClientResponse (Error msg)) =
    io_ $ consoleError ("Main Component ClientResponse decode failure: " <> toMisoString msg)

mainUpdate (GoToTime t) = do
    modify (\m -> m { current_time = t })
    publish Client.clientInTopic (SenderLatest, Client.FetchLatest t)

mainUpdate (GetThread Client.GetThreadArgs {..}) = do
    io_ $ consoleLog $ "Thread " `append` (pack $ show $ board_thread_id)

    model <- get

    io_ $ pushURI $ new_current_uri model

    publish Client.clientInTopic (SenderThread, Client.GetThread Client.GetThreadArgs {..})

    where
        new_current_uri :: Model -> URI
        new_current_uri m = (current_uri m)
            { uriPath = T.unpack website
                    </> T.unpack board_pathpart
                    </> show board_thread_id
            , uriQuery = ""
            }

mainUpdate (ChangeURI uri) = do
    modify (\m -> m { current_uri = uri })
    io_ $ consoleLog $ "ChangeURI! " `append` (pack $ show $ uri)


mainUpdate (SearchResults query) = do
    model <- get

    let new_uri :: URI = new_current_uri model

    io_ $ do
        consoleLog $ "SearchResults new uri: " <> (pack $ show $ new_uri)
        pushURI new_uri

    put model { current_uri = new_uri }

    where
        new_current_uri :: Model -> URI
        new_current_uri m = (current_uri m)
            { uriPath = "/search"
            , uriQuery = "?search=" ++ (escapeURIString isAllowedInURI $ unpack query)
            }

mainUpdate (NotifySearch query) = publish Search.searchTopic query
