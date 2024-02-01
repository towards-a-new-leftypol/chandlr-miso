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
    , Model (..)
    , update
    ) where

import GHC.Generics
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)
import Data.Aeson (ToJSON)
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime)

import GHCJS.DOM.Types (JSString)
import Miso (effectSub, Effect)

import qualified Network.Http as Http
import Network.CatalogPostType (CatalogPost)
import qualified Action as A
import Network.ClientTypes


update
    :: Interface a
    -> Action
    -> Model
    -> Effect a Model
update iface (Connect (abort, resultVar)) m = effectSub m $
    \sink -> void $ forkIO $ do
        result :: Http.HttpResult [CatalogPost] <- takeMVar resultVar
        sink $ (returnResult iface) result

data FetchCatalogArgs = FetchCatalogArgs
  { max_time :: UTCTime
  , max_row_read :: Int
  } deriving (Generic, ToJSON)


http_
    :: (ToJSON b)
    => Model
    -> Interface a
    -> JSString
    -> Http.HttpMethod
    -> Maybe b
    -> IO a
http_ m iface api_path method payload = do
    Http.http
        (pgApiRoot m <> api_path)
        method
        [("Content-Type", "application/json")]
        payload
    >>= return . (passAction iface) . Connect


fetchLatest :: Model -> Interface a -> IO a
fetchLatest m iface = do
    ct <- getCurrentTime

    Http.http
        ((pgApiRoot m) <> ("/rpc/fetch_catalog" :: JSString))
        Http.POST
        [("Content-Type", "application/json")]
        ( Just $ FetchCatalogArgs
            { max_time = ct
            , max_row_read = fetchCount m
            }
        )
    >>= return . (passAction iface) . Connect


getThread :: A.GetThreadArgs -> IO a
getThread A.GetThreadArgs {..} = undefined


-- TODO: Action.GetLatest needs to be refactored out into a shared
--      data structure that we can pass as the argument for this getThread
--      function
