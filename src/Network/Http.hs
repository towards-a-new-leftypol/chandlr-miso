{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Http
    ( http
    , HttpActionResult
    , HttpMethod (..)
    , HttpResult (..)
    )
where

import Prelude hiding (error)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Miso.String (MisoString, toMisoString)
import Miso (consoleLog, consoleError)
import Miso.JSON (ToJSON, eitherDecode, encode)

import Common.Network.HttpTypes
import JSFFI.XHR
    ( XMLHttpRequest(..)
    , abort
    , send
    , setRequestHeader
    , open
    , newXMLHttpRequest
    , addEventListener
    , getStatusText
    , getResponseText
    , getStatus
    )

type Header = (MisoString, MisoString)

mkResult :: XMLHttpRequest -> IO HttpResult
mkResult xhr = do
        status_code_int <- getStatus xhr

        st :: MisoString <- getStatusText xhr

        mResponseStr :: Maybe MisoString <- getResponseText xhr

        if status_code_int >= 200 && status_code_int < 300
        then
            case mResponseStr of
                Nothing -> return HttpResponse
                        { status_code = status_code_int
                        , status_text = st
                        , body = Nothing
                        }
                Just response -> do
                    let parse_result = eitherDecode response
                    case parse_result of
                        Left err -> do
                          consoleError $ toMisoString err
                          return $ Error $ toMisoString err
                        Right x -> do
                            consoleLog "Decoding Successful"
                            return HttpResponse
                                { status_code = status_code_int
                                , status_text = st
                                , body = Just x
                                }
        else
            return $ Error $
                "Server responded with " <> (toMisoString $ show status_code_int)
                <> " " <> st
                <> maybe "" (\m -> "\nDetails:\n" <> m) mResponseStr


http
    :: ToJSON a
    => MisoString
    -> HttpMethod
    -> [Header]
    -> Maybe a
    -> IO HttpActionResult
http url method headers payload = do
    xhr <- newXMLHttpRequest

    resultVar <- liftIO $ newEmptyMVar

    addEventListener (jsval_ xhr) "load" $ do
        result <- mkResult xhr
        liftIO $ putMVar resultVar result

    addEventListener (jsval_ xhr) "abortEvent" $ liftIO $
        putMVar resultVar $ Error "Request aborted."

    addEventListener (jsval_ xhr) "error" $ liftIO $
        putMVar resultVar $ Error "Network Error"

    open xhr (toMisoString $ show method) url
    -- "/posts?limit=10"

    mapM_ (\(k, v) -> setRequestHeader xhr k v) headers

    let p = payload >>= Just . encode

    send xhr p
    return (abort xhr, resultVar)


    where
        jsval_ (XMLHttpRequest x) = x
