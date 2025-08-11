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

import Data.Maybe (fromJust)
import Language.Javascript.JSaddle
    ( JSVal
    , JSM
    , (#)
    , ToJSVal (..)
    , jsg
    , (!)
    , new
    , asyncFunction
    , FromJSVal (..)
    )
import Miso.String (MisoString)


newtype XMLHttpRequest = XMLHttpRequest JSVal


newXMLHttpRequest :: JSM XMLHttpRequest
newXMLHttpRequest = XMLHttpRequest <$> new (jsg ("XMLHttpRequest" :: MisoString)) ()


abort :: XMLHttpRequest -> JSM ()
abort (XMLHttpRequest xhr) = do
    _ <- xhr # ("abort" :: MisoString) $ ([] :: [ MisoString ])
    return ()


send :: (ToJSVal a) => XMLHttpRequest -> a -> JSM ()
send (XMLHttpRequest xhr) payload = do
    _ <- xhr # ("send" :: MisoString) $ ([ toJSVal payload ])
    return ()


setRequestHeader :: XMLHttpRequest -> MisoString -> MisoString -> JSM ()
setRequestHeader (XMLHttpRequest xhr) k v = do
    _ <- xhr # ("setRequestHeader" :: MisoString) $ ([ k, v ])
    return ()


open :: XMLHttpRequest -> MisoString -> MisoString -> JSM ()
open (XMLHttpRequest xhr) method url = do
    _ <- xhr # ("open" :: MisoString) $ ([ method, url ])
    return ()


getStatusText :: XMLHttpRequest -> JSM MisoString
getStatusText (XMLHttpRequest xhr) =
    getProp_ xhr "statusText" >>= return . fromJust


getResponseText :: XMLHttpRequest -> JSM (Maybe MisoString)
getResponseText (XMLHttpRequest xhr) = getProp_ xhr "responseText"


getStatus :: XMLHttpRequest -> JSM Int
getStatus (XMLHttpRequest xhr) = getProp_ xhr "status" >>= return . fromJust


addEventListener
  :: JSVal
  -> MisoString
  -> JSM ()
  -> JSM ()
addEventListener self name cb = do
    _ <- self # ("addEventListener" :: MisoString) $ (name, asyncFunction handle)
    return ()

    where
      handle _ _ _ = cb


getProp_ :: (FromJSVal a) => JSVal -> MisoString -> JSM (Maybe a)
getProp_ self name = self ! name >>= fromJSVal
