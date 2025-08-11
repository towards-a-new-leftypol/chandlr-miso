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
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Data.Aeson (ToJSON, eitherDecodeStrictText)
import Data.Aeson.Text (encodeToLazyText)
import Miso.String (MisoString, toMisoString, fromMisoString)
import Miso (consoleLog)
import Language.Javascript.JSaddle.Monad (JSM)

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

mkResult :: XMLHttpRequest -> JSM HttpResult
mkResult xhr = do
        status_code_int <- getStatus xhr

        st :: String <- fromMisoString <$> getStatusText xhr

        mText :: Maybe Text <- (fromMisoString <$>) <$> getResponseText xhr

        case mText of
            Nothing -> return HttpResponse
                    { status_code = status_code_int
                    , status_text = st
                    , body = Nothing
                    }
            Just response -> do
                let parse_result = eitherDecodeStrictText response
                case parse_result of
                    Left err -> do
                      consoleLog $ toMisoString $ show err
                      return Error
                    Right x -> do
                        consoleLog "Decoding Successful"
                        return HttpResponse
                            { status_code = status_code_int
                            , status_text = st
                            , body = Just x
                            }


http
    :: ToJSON a
    => MisoString
    -> HttpMethod
    -> [Header]
    -> Maybe a
    -> JSM HttpActionResult
http url method headers payload = do
    xhr <- newXMLHttpRequest

    resultVar <- liftIO $ newEmptyMVar

    addEventListener (jsval_ xhr) "load" $ do
        result <- mkResult xhr
        liftIO $ putMVar resultVar result

    addEventListener (jsval_ xhr) "abortEvent" $ liftIO $
        putMVar resultVar Error

    addEventListener (jsval_ xhr) "error" $ liftIO $
        putMVar resultVar Error

    open xhr (toMisoString $ show method) url
    -- "/posts?limit=10"

    mapM_ (\(k, v) -> setRequestHeader xhr k v) headers

    let p = payload >>= Just . toMisoString . toStrict . encodeToLazyText

    send xhr p
    return (abort xhr, resultVar)


    where
        jsval_ (XMLHttpRequest x) = x
