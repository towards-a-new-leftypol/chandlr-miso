{-# LANGUAGE ScopedTypeVariables #-}

module Network.Client
    ( Http.HttpActionResult
    , Http.HttpMethod (..)
    , Http.HttpResult (..)
    , Action (..)
    , Interface (..)
    , fetchLatest
    , Model
    , update
    , initialModel
    ) where

import Data.Text (Text)
import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)

import Miso (effectSub, Effect)

import qualified Network.Http as Http
import Common.PostsType (Post)


data Action = Connect (Http.HttpActionResult [Post])


data Interface a = Interface
    { passAction :: Action -> a
    , returnResult :: Http.HttpResult [Post] -> a
    }


type Model = ()

initialModel :: Model
initialModel = ()


update
    :: Interface a
    -> Action
    -> Model
    -> Effect a Model
update iface (Connect (abort, resultVar)) m = effectSub m $
    \sink -> void $ forkIO $ do
        result :: Http.HttpResult [Post] <- takeMVar resultVar
        sink $ (returnResult iface) result


fetchLatest :: Interface a -> IO a
fetchLatest iface =
    Http.http
        "http://localhost:3000/posts?limit=10"
        Http.GET
        Nothing

    >>= return . (passAction iface) . Connect
