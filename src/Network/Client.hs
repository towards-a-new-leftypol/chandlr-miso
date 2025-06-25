{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Network.Client
    ( Http.HttpActionResult
    , Http.HttpMethod (..)
    , Http.HttpResult (..)
    , Action
    , ActionVerb (..)
    , Interface (..)
    , Model (..)
    , update
    , app
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad.State (get)
import GHC.TypeLits (KnownSymbol)

import Miso (withSink, Effect, io_, notify, Component, text)
import qualified Miso as M
import Miso.String (MisoString, toMisoString)
import Language.Javascript.JSaddle.Monad (askJSM, runJSaddle)
import Language.Javascript.JSaddle.Monad (JSM)

import qualified Network.Http as Http
import Common.Network.ClientTypes
import qualified Common.FrontEnd.Action as A

awaitResult
    :: KnownSymbol n
    => Interface n m a b
    -> Http.HttpActionResult b
    -> Effect Model (Action n m a b)
awaitResult iface (_, resultVar) = do
    io_ $ do
        ctx <- askJSM

        void $ liftIO $ forkIO $ do
            result :: Http.HttpResult b <- takeMVar resultVar
            --runJSaddle ctx $ sink $ (returnResult iface) result
            runJSaddle ctx $
                notify (notifyComponent iface) $ (returnResult iface) result

update :: (FromJSON b, KnownSymbol n) => Action n m a b -> Effect Model (Action n m a b)
update (iface, Connect actionResult) = awaitResult iface actionResult
update (iface, FetchLatest t) = do
    model <- get

    let payload = Just $ FetchCatalogArgs
            { max_time = t
            , max_row_read = fetchCount model
            }

    send iface $ http_ model "/rpc/fetch_catalog" Http.POST payload

update (iface, GetThread A.GetThreadArgs {..}) = do
    model <- get

    send iface $ http_ model path Http.GET (Nothing :: Maybe ())

    where
        path = "/sites?"
            <> "select=*,boards(*,threads(*,posts(*,attachments(*))))"
            <> "&name=eq." <> toMisoString website
            <> "&boards.pathpart=eq." <> toMisoString board_pathpart
            <> "&boards.threads.board_thread_id=eq." <> toMisoString (show board_thread_id)
            <> "&boards.threads.posts.order=board_post_id.asc"

update (iface, Search query) = do
    model <- get

    send iface $ http_ model "/rpc/search_posts" Http.POST payload

    where
        payload = Just $ SearchPostsArgs
            { search_text = query
            , max_rows = 100
            }

send
    :: Interface n m a b
    -> JSM (Http.HttpActionResult b)
    -> Effect Model (Action n m a b)
send iface action =
    withSink $ \sink ->
        action
        >>= sink . ((,) iface) . Connect

http_
    :: (ToJSON a, FromJSON b)
    => Model
    -> MisoString
    -> Http.HttpMethod
    -> Maybe a
    -> JSM (Http.HttpActionResult b)
http_ m apiPath method payload =
    Http.http
        (pgApiRoot m <> apiPath)
        method
        [("Content-Type", "application/json")]
        payload


app :: (FromJSON b, KnownSymbol n) => Component "http-client" Model (Action n m a b)
app = M.Component
    { M.model = Uninitialized
    , M.update = update
    , M.view = const $ text ""
    , M.subs = []
    , M.events = M.defaultEvents
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    }
