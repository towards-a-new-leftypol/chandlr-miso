{-# LANGUAGE OverloadedStrings #-}

module JSFFI.XHR
    ( XMLHttpRequest (..)
    , abort
    , send
    , setRequestHeader
    , open
    , newXMLHttpRequest
    , addEventListener
    , getStatusText
    , getResponseText
    , getStatus
    ) where

import Miso.DSL
    ( JSVal
    , jsg
    , (#)
    , (!)
    , new
    , toJSVal
    , fromJSVal
    , asyncCallback
    , ToJSVal
    , FromJSVal
    )
import Miso.String (MisoString)
import Data.Maybe (fromJust)
import Control.Monad (void)

newtype XMLHttpRequest = XMLHttpRequest JSVal

newXMLHttpRequest :: IO XMLHttpRequest
newXMLHttpRequest = XMLHttpRequest <$> new (jsg "XMLHttpRequest") ([] :: [ JSVal ])

abort :: XMLHttpRequest -> IO ()
abort (XMLHttpRequest xhr) = void $ xhr # "abort" $ ([] :: [ JSVal ])

send :: ToJSVal a => XMLHttpRequest -> a -> IO ()
send (XMLHttpRequest xhr) payload = do
    val <- toJSVal payload
    void $ xhr # "send" $ [val]

setRequestHeader :: XMLHttpRequest -> MisoString -> MisoString -> IO ()
setRequestHeader (XMLHttpRequest xhr) k v =
    void $ xhr # "setRequestHeader" $ [k, v]

open :: XMLHttpRequest -> MisoString -> MisoString -> IO ()
open (XMLHttpRequest xhr) method url =
    void $ xhr # "open" $ [method, url]

getStatusText :: XMLHttpRequest -> IO MisoString
getStatusText (XMLHttpRequest xhr) =
    getProp_ xhr "statusText" >>= return . fromJust

getResponseText :: XMLHttpRequest -> IO (Maybe MisoString)
getResponseText (XMLHttpRequest xhr) = getProp_ xhr "responseText"

getStatus :: XMLHttpRequest -> IO Int
getStatus (XMLHttpRequest xhr) = getProp_ xhr "status" >>= return . fromJust

addEventListener :: JSVal -> MisoString -> IO () -> IO ()
addEventListener self name cb = do
    cbVal <- asyncCallback cb
    void ((#) self "addEventListener" (name, cbVal))

getProp_ :: FromJSVal a => JSVal -> MisoString -> IO (Maybe a)
getProp_ self name = self ! name >>= fromJSVal
