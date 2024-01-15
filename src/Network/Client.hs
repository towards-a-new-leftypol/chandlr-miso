{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

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


data Action = Connect (Http.HttpActionResult [CatalogPost])


data Interface a = Interface
    { passAction :: Action -> a
    , returnResult :: Http.HttpResult [CatalogPost] -> a
    }

data Model = Model
  { pgApiRoot :: JSString
  , fetchCount :: Int
  } deriving Eq


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
