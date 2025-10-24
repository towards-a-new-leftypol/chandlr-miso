{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Network.Client
    ( Http.HttpActionResult
    , Http.HttpMethod (..)
    , Http.HttpResult (..)
    , Action (..)
    , Model (..)
    , update
    , app
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)
import Data.Aeson (ToJSON)
import Control.Monad.State (get, put)

import Miso
    ( withSink
    , Effect
    , Component
    , text
    , io, io_
    , consoleError
    , checkMail
    , mail
    )
import qualified Miso as M
import Miso.String (MisoString, toMisoString)
import Language.Javascript.JSaddle.Monad (askJSM, runJSaddle)

import qualified Network.Http as Http
import Common.Network.ClientTypes


awaitResult
    :: Http.HttpActionResult
    -> Sender
    -> Effect parent Model Action
awaitResult (_, resultVar) sender =
    withSink $ \sink -> do
        ctx <- askJSM

        void $ liftIO $ forkIO $ do
            result <- takeMVar resultVar
            --runJSaddle ctx $ sink $ (returnResult iface) result
            runJSaddle ctx $
                sink $ mail sender result


update :: Action -> Effect parent Model Action
update (Connect sender actionResult) = awaitResult actionResult sender
update (OnMessage (_, InitModel m)) = put m
update (OnMessage (sender, FetchLatest t)) = do
    model <- get

    let payload = Just $ FetchCatalogArgs
            { max_time = t
            , max_row_read = fetchCount model
            }

    http_ model "/rpc/fetch_catalog" Http.POST payload sender

update (OnMessage (sender, GetThread GetThreadArgs {..})) = do
    model <- get

    http_ model path Http.GET (Nothing :: Maybe ()) sender

    where
        path = "/sites?"
            <> "select=*,boards(*,threads(*,posts(*,attachments(*))))"
            <> "&name=eq." <> toMisoString website
            <> "&boards.pathpart=eq." <> toMisoString board_pathpart
            <> "&boards.threads.board_thread_id=eq." <> toMisoString (show board_thread_id)
            <> "&boards.threads.posts.order=board_post_id.asc"

update (OnMessage (sender, Search query)) = do
    model <- get

    http_ model "/rpc/search_posts" Http.POST payload sender

    where
        payload = Just $ SearchPostsArgs
            { search_text = query
            , max_rows = 100
            }

update (OnErrorMessage msg) =
    io_ $ consoleError ("Client Message decode failure: " <> toMisoString msg)


http_
    :: (ToJSON a)
    => Model
    -> MisoString
    -> Http.HttpMethod
    -> Maybe a
    -> Sender
    -- -> JSM (Http.HttpActionResult b)
    -> Effect parent Model Action
http_ m apiPath method payload sender =
    io $ Connect sender <$> Http.http
        (pgApiRoot m <> apiPath)
        method
        [("Content-Type", "application/json")]
        payload


app :: Component parent Model Action
app = M.Component
    { M.model = Uninitialized
    , M.hydrateModel = Nothing
    , M.update = update
    , M.view = const $ text ""
    , M.subs = []
    , M.events = M.defaultEvents
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = checkMail OnMessage OnErrorMessage
    , M.bindings = []
    }
