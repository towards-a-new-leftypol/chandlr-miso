{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

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
    , notify
    , component_
    , onMountedWith
    , issue
    , key_
    )
import Miso.String (MisoString, toMisoString)
import Servant.API
import Data.Aeson (decodeStrict, FromJSON)
import Control.Monad.IO.Class (liftIO)
import Language.Javascript.JSaddle (toJSString)
import Language.Javascript.JSaddle.Monad (JSM)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Common.FrontEnd.Action
import Common.FrontEnd.Routes
import qualified Network.Client as Client
import qualified Common.Component.Thread as Thread
import qualified Common.Component.TimeControl as TC
import qualified Common.Component.Search.SearchTypes as Search
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
import qualified Common.Network.CatalogPostType as CatP
import qualified Common.Network.HttpTypes as Http

import Debug.Trace (trace)

data InitialData
    = CatalogData [ CatalogPost ]
    | SearchData [ CatalogPost ]
    | ThreadData Site
    | Nil


parseInitialDataUsingRoute :: Model -> URI -> MisoString -> InitialData
parseInitialDataUsingRoute model uri raw_json = either (const Nil) id routing_result
    where
        decoded_thing :: (FromJSON a) => Maybe a
        decoded_thing = decodeStrict $ encodeUtf8 $ textFromJSString raw_json

        routing_result = route (Proxy :: Proxy Route) handlers (const uri) model

        -- handlers = h_latest :<|> h_thread :<|> h_search
        handlers = h_latest

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

        -- handlers = h_latest :<|> h_thread :<|> h_search
        handlers = h_latest

        h_latest :: Model -> Action
        h_latest = const $ GoToTime $ current_time model

        h_thread :: Text -> Text -> BoardThreadId -> Model -> Action
        h_thread website board_pathpart board_thread_id _ = GetThread GetThreadArgs {..}

        h_search :: Maybe Text -> Model -> Action
        h_search Nothing m = GoToTime $ current_time m
        h_search (Just search_query) m
            | search_term m == unescaped_search_query = SearchResults unescaped_search_query
            | otherwise = NotifySearch $ Search.ChangeAndSubmit unescaped_search_query

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
    , initial_action = NoAction
    , thread_action = Nothing
    , grid_action = Nothing
    , pg_api_root = pgroot
    , client_fetch_count = fetch_count
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

epochToUTCTime :: Int -> UTCTime
epochToUTCTime = posixSecondsToUTCTime . realToFrac

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

    let testPost = CatP.CatalogPost
            { CatP.post_id              = Just 0
            , CatP.board_post_id        = 0
            , CatP.board_thread_id      = 0
            , CatP.creation_time        = epochToUTCTime 0
            , CatP.bump_time            = epochToUTCTime 0
            , CatP.body                 = Just "Holy Shit"
            , CatP.name                 = Nothing
            , CatP.subject              = Nothing
            , CatP.email                = Nothing
            , CatP.thread_id            = 0
            , CatP.embed                = Nothing
            , CatP.estimated_post_count = 0
            , CatP.site_name            = "test"
            , CatP.pathpart             = "b"
            , CatP.file_mimetype        = Nothing
            , CatP.file_illegal         = Nothing
            , CatP.file_name            = Nothing
            , CatP.file_extension       = Nothing
            , CatP.file_thumb_extension = Nothing
            }

        -- HaveLatest (HttpResult [ CatalogPost ])

    let initAction = HaveLatest $ Http.HttpResponse 100 "OK" (Just [testPost])

    let app :: MainComponent = Component
            -- { model         = initial_model { initial_action = (initialActionFromRoute initial_model uri) }
            { model         = initial_model
            , update        = mainUpdate app
            , view          = mainView app
            , subs          = [ uriSub ChangeURI ]
            , events        = defaultEvents
            , styles = []
            , initialAction = Just initAction
            , mountPoint    = Nothing
            , logLevel      = DebugAll
            }

    startComponent app


addToView :: View action -> View action -> View action
addToView child (VNode a b cs ds) = VNode a b cs (child : ds)
addToView _ v = v


mainView :: MainComponent -> Model -> View Action
mainView mc model = view
    where
        view = either (const page404) id $
            route (Proxy :: Proxy Route) handlers current_uri model

        addClient :: View Action -> View Action
        addClient = addToView $
            component_
                Client.app
                [ key_ "http-client"
                , onMountedWith (const ClientMounted)
                ]

        handlers
            =    (catalogView tc grid)
            -- :<|> (threadView Thread.app)
            -- :<|> (searchView grid)

        tc :: TC.TimeControl
        tc = TC.app 0 timeCallback

        timeCallback :: TC.TimeChangeCallback "body" Model Action
        timeCallback = (mc, GoToTime)

        grid :: Grid.GridComponent
        grid = Grid.app mc (media_root_ model)


mainUpdate :: MainComponent -> Action -> Effect Model Action
mainUpdate _ ClientMounted = do
    model <- get

    io_ $ do
        consoleLog "Http Client Mounted!"
        notify
            Client.app
            ( undefined
            , Client.InitModel $
                Client.Model
                    (pg_api_root model)
                    (client_fetch_count model)
            )

    issue $ initial_action model

mainUpdate _ ThreadViewMounted = do
    io_ $ consoleLog "ThreadViewMounted"

    model <- get

    maybe
        (return ())
        (io_ . notify Thread.app)
        (thread_action model)

mainUpdate _ (HaveLatest Client.Error) =
    io_ $ consoleLog "Getting Latest failed!"

mainUpdate _ (HaveLatest (Client.HttpResponse {..})) =
    case body of
        Nothing -> io_ $
            consoleLog "Didn't get anything back from API"
        Just posts -> do
            io_ $ do
                mapM_ (consoleLog . toJSString . show) posts
                -- notify (Grid.app undefined undefined) $ Grid.DisplayItems posts

            -- here we need to modify our model to save these catalog posts as an action for Grid
            modify (\m -> m { grid_action = Just $ Grid.DisplayItems posts })

mainUpdate _ (HaveThread Client.Error) =
    io_ $ consoleLog "Getting Thread failed!"

mainUpdate _ (HaveThread (Client.HttpResponse {..})) = do
    io_ $ consoleLog "Have Thread!"
    -- modify
    --     ( \m -> m
    --         { thread_action = Just $
    --             Thread.RenderSite (media_root_ m) (head $ fromJust body)
    --         }
    --     )

mainUpdate mc (GoToTime t) = do
  modify (\m -> m { current_time = t })
  io_ $ notify Client.app (iface, Client.FetchLatest t)

  where
    iface :: Client.SomeInterface
    iface = Client.SomeInterface $
        Client.Interface HaveLatest mc

mainUpdate mc (GetThread GetThreadArgs {..}) = do
    io_ $ consoleLog $ "Thread " `append` (pack $ show $ board_thread_id)

    model <- get

    io_ $ do
        -- pushURI $ new_current_uri model
        consoleLog $ "pushURI: " <> (toMisoString $ show $ new_current_uri model)

    put (trace "MODEL MODEL MODEL MODEL " model) { current_uri = (current_uri model) { uriPath = "/asdf" } }

        -- notify Client.app (iface, Client.GetThread GetThreadArgs {..})

    where
        iface :: Client.SomeInterface
        iface = Client.SomeInterface $
            Client.Interface HaveThread mc

        new_current_uri :: Model -> URI
        new_current_uri m = (current_uri m)
            { uriPath = T.unpack website
                    </> T.unpack board_pathpart
                    </> show board_thread_id
            , uriQuery = ""
            }

mainUpdate _ (ChangeURI uri) = do
    io_ $ consoleLog $ "mainUpdate - ChangeURI! " `append` (toMisoString $ show $ uri)
    --modify (\m -> m { current_uri = current_uri m })
    modify (\m -> m { current_uri = uri { uriPath = "/asdf" } })


mainUpdate _ (SearchResults query) = do
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

mainUpdate _ NoAction = io_ $ consoleLog "Main - NoAction"

mainUpdate mc GridMounted = do
    io_ $ consoleLog "Main - GridMounted"

    model <- get

    maybe
        (return ())
        (io_ . notify (Grid.app mc (media_root_ model)))
        (grid_action model)
