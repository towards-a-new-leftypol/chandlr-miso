{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Client
    ( Http.HttpActionResult
    , Http.HttpMethod (..)
    , Http.HttpResult (..)
    , Action (..)
    , Interface (..)
    , fetchLatest
    , getThread
    , Model (..)
    , update
    , search
    ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)

import Miso (effectSub, Effect, JSM)
import Miso.String (MisoString, toMisoString)
import Language.Javascript.JSaddle.Monad (askJSM, runJSaddle)

import qualified Network.Http as Http
import Common.Network.CatalogPostType (CatalogPost)
import Common.Network.SiteType (Site)
import qualified Common.FrontEnd.Action as A
import Common.Network.ClientTypes

update
    :: Interface a b
    -> Action b
    -> Model
    -> Effect Model a ()
update iface (Connect (_, resultVar)) m =
    effectSub m $ \sink -> do
        ctx <- askJSM

        void $ liftIO $ forkIO $ do
            result :: Http.HttpResult b <- takeMVar resultVar
            runJSaddle ctx $ sink $ (returnResult iface) result

http_
    :: (ToJSON c, FromJSON b)
    => Model
    -> Interface a b
    -> MisoString
    -> Http.HttpMethod
    -> Maybe c
    -> JSM a
http_ m iface api_path method payload =
    Http.http
        (pgApiRoot m <> api_path)
        method
        [("Content-Type", "application/json")]
        payload
    >>= return . (passAction iface) . Connect


fetchLatest :: Model -> UTCTime -> Interface a [ CatalogPost ] -> JSM a
fetchLatest m t iface = do
    let payload = Just $ FetchCatalogArgs
            { max_time = t
            , max_row_read = fetchCount m
            }

    http_ m iface "/rpc/fetch_catalog" Http.POST payload


getThread :: Model -> Interface a [ Site ] -> A.GetThreadArgs -> JSM a
getThread m iface A.GetThreadArgs {..} =
    http_ m iface path Http.GET (Nothing :: Maybe ())

    where
        path = "/sites?"
            <> "select=*,boards(*,threads(*,posts(*,attachments(*))))"
            <> "&name=eq." <> toMisoString website
            <> "&boards.pathpart=eq." <> toMisoString board_pathpart
            <> "&boards.threads.board_thread_id=eq." <> toMisoString (show board_thread_id)
            <> "&boards.threads.posts.order=board_post_id.asc"


search :: Model -> MisoString -> Interface a [ CatalogPost ] -> JSM a
search m query iface =
    http_ m iface "/rpc/search_posts" Http.POST payload

    where
        payload = Just $ SearchPostsArgs
            { search_text = query
            , max_rows = 100
            }

