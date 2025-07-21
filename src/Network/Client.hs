{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Client
    ( Http.HttpActionResult
    , Http.HttpMethod (..)
    , Http.HttpResult (..)
    , Action (..)
    , Model (..)
    , update
    , app
    , helper
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)
import Data.Aeson (ToJSON, FromJSON, fromJSON, Result(..))
import Control.Monad.State (get, put)

import Miso
    ( withSink
    , Effect
    , Component
    , div_
    , publish
    , io, io_
    , subscribe
    , consoleError
    , consoleLog
    , key_
    )
import qualified Miso as M
import Miso.String (MisoString, toMisoString)
import Language.Javascript.JSaddle.Monad (askJSM, runJSaddle)

import qualified Network.Http as Http
import Common.Network.ClientTypes


awaitResult
    :: Http.HttpActionResult
    -> Sender
    -> Effect Model Action
awaitResult (_, resultVar) sender =
    withSink $ \sink -> do
        ctx <- askJSM

        void $ liftIO $ forkIO $ do
            result <- ReturnResult sender <$> takeMVar resultVar
            --runJSaddle ctx $ sink $ (returnResult iface) result
            runJSaddle ctx $
                sink $ Publish result


update :: Action -> Effect Model Action
update Initialize = subscribe clientInTopic OnMessage
update OnMount = do
    io_ $ consoleLog "Client OnMount (publishing topic)"
    publish clientOutTopic Mounted
update OnUnmount = publish clientOutTopic Unmounted
update (Publish x) = publish clientOutTopic x
update (OnMessage (Success (_, InitModel m))) = put m
update (Connect sender actionResult) = awaitResult actionResult sender
update (OnMessage (Success (sender, FetchLatest t))) = do
    model <- get

    let payload = Just $ FetchCatalogArgs
            { max_time = t
            , max_row_read = fetchCount model
            }

    http_ model "/rpc/fetch_catalog" Http.POST payload sender

update (OnMessage (Success (sender, GetThread GetThreadArgs {..}))) = do
    model <- get

    http_ model path Http.GET (Nothing :: Maybe ()) sender

    where
        path = "/sites?"
            <> "select=*,boards(*,threads(*,posts(*,attachments(*))))"
            <> "&name=eq." <> toMisoString website
            <> "&boards.pathpart=eq." <> toMisoString board_pathpart
            <> "&boards.threads.board_thread_id=eq." <> toMisoString (show board_thread_id)
            <> "&boards.threads.posts.order=board_post_id.asc"

update (OnMessage (Success (sender, Search query))) = do
    model <- get

    http_ model "/rpc/search_posts" Http.POST payload sender

    where
        payload = Just $ SearchPostsArgs
            { search_text = query
            , max_rows = 100
            }

update (OnMessage (Error msg)) =
    io_ $ consoleError ("Client Message decode failure: " <> toMisoString msg)


http_
    :: (ToJSON a)
    => Model
    -> MisoString
    -> Http.HttpMethod
    -> Maybe a
    -> Sender
    -- -> JSM (Http.HttpActionResult b)
    -> Effect Model Action
http_ m apiPath method payload sender =
    io $ Connect sender <$> Http.http
        (pgApiRoot m <> apiPath)
        method
        [("Content-Type", "application/json")]
        payload


app :: Component Model Action
app = M.Component
    { M.model = Uninitialized
    , M.update = update
    , M.view = view
    , M.subs = []
    , M.events = M.defaultEvents
    , M.styles = []
    , M.initialAction = Just Initialize
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    }


view :: a -> M.View Action
view = const $
    div_
        [ M.onMounted OnMount
        , M.onUnmounted OnUnmount
        , key_ ("http-client" :: MisoString)
        ]
        [ "Http - Client" ]

-- But we probably want an Effect here
helper
    :: (FromJSON a)
    => Http.HttpResult
    -> (a -> Effect model action)
    -> Effect model action
helper Http.Error _ = io_ $ consoleError "Http Error"
helper (Http.HttpResponse status_code status_text (Just body)) continue = do
    io_ $ do
        consoleLog $ (toMisoString $ show $ status_code) <> " " <> (toMisoString $ status_text)
        consoleLog $ (toMisoString $ show $ body)

    let parsed = fromJSON body

    case parsed of
        Error msg -> io_ $ consoleError (toMisoString msg) -- alert Error component here, maybe have toast pop up
        Success x -> continue x

helper _ _ = return () -- No body, nothing to parse
