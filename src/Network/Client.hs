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
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time.Clock (UTCTime)

import GHCJS.DOM.Types (JSString)
import Miso (effectSub, Effect)
import Miso.String (toMisoString)

import qualified Network.Http as Http
import Common.Network.CatalogPostType (CatalogPost)
import Common.Network.SiteType (Site)
import qualified Common.FrontEnd.Action as A
import Common.Network.ClientTypes

update
    :: Interface a b
    -> Action b
    -> Model
    -> Effect a Model
update iface (Connect (abort, resultVar)) m = effectSub m $
    \sink -> void $ forkIO $ do
        result :: Http.HttpResult b <- takeMVar resultVar
        sink $ (returnResult iface) result

http_
    :: (ToJSON c, FromJSON b)
    => Model
    -> Interface a b
    -> JSString
    -> Http.HttpMethod
    -> Maybe c
    -> IO a
http_ m iface api_path method payload =
    Http.http
        (pgApiRoot m <> api_path)
        method
        [("Content-Type", "application/json")]
        payload
    >>= return . (passAction iface) . Connect


fetchLatest :: Model -> UTCTime -> Interface a [ CatalogPost ] -> IO a
fetchLatest m t iface = do
    let payload = Just $ FetchCatalogArgs
            { max_time = t
            , max_row_read = fetchCount m
            }

    http_ m iface "/rpc/fetch_catalog" Http.POST payload


getThread :: Model -> Interface a [ Site ] -> A.GetThreadArgs -> IO a
getThread m iface A.GetThreadArgs {..} =
    http_ m iface path Http.GET (Nothing :: Maybe ())

    where
        path = "/sites?"
            <> "select=*,boards(*,threads(*,posts(*,attachments(*))))"
            <> "&name=eq." <> toMisoString website
            <> "&boards.pathpart=eq." <> toMisoString board_pathpart
            <> "&boards.threads.board_thread_id=eq." <> toMisoString (show board_thread_id)
            <> "&boards.threads.posts.order=board_post_id.asc"


search :: Model -> JSString -> Interface a [ CatalogPost ] -> IO a
search m query iface =
    http_ m iface "/rpc/search_posts" Http.POST payload

    where
        payload = Just $ SearchPostsArgs
            { search_text = query
            , max_rows = 100
            }

