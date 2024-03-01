{-# LANGUAGE ScopedTypeVariables #-}

module Network.Http
    ( http
    , HttpActionResult
    , HttpMethod (..)
    , HttpResult (..)
    )
where

import Prelude hiding (error)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict, encode)
import GHCJS.DOM.XMLHttpRequest
    ( newXMLHttpRequest
    , openSimple
    , getStatus
    , getStatusText
    , getResponseText
    , abort
    , setRequestHeader
    -- , send
    )
import GHCJS.DOM.JSFFI.Generated.XMLHttpRequest (send)
import GHCJS.DOM.Types (XMLHttpRequest, JSString)
import Data.JSString.Text (textToJSString)
import GHCJS.DOM.EventM (onAsync)
import GHCJS.DOM.XMLHttpRequestEventTarget (load, abortEvent, error)
import GHCJS.DOM.Types (toJSString)
import Miso (consoleLog)

data HttpMethod = GET | PUT | POST | DELETE | PATCH
    deriving Show


data HttpResult a
    = Error
    | HttpResponse
        { status_code :: Int
        , status_text :: String
        , body        :: Maybe a
        }

type HttpActionResult a = (IO (), MVar (HttpResult a)) -- (abort, result)

type Header = (JSString, JSString)

mkResult :: (FromJSON a) => XMLHttpRequest -> IO (HttpResult a)
mkResult xhr = do
        sc <- getStatus xhr

        let status_code_int :: Int = fromEnum sc

        st :: String <- getStatusText xhr

        mBody :: Maybe Text <- getResponseText xhr

        let mBytes = mBody >>= Just . encodeUtf8

        case mBytes of
            Nothing -> return HttpResponse
                    { status_code = status_code_int
                    , status_text = st
                    , body = Nothing
                    }
            Just bs -> do
                let parse_result = eitherDecodeStrict bs
                case parse_result of
                    Left err -> do
                      consoleLog $ toJSString $ show err
                      return Error
                    Right x -> do
                        consoleLog $ toJSString "Decoding Successful"
                        return HttpResponse
                            { status_code = status_code_int
                            , status_text = st
                            , body = Just x
                            }


http
    :: (FromJSON a, ToJSON b)
    => JSString
    -> HttpMethod
    -> [Header]
    -> Maybe b
    -> IO (HttpActionResult a)
http url method headers payload = do
    xhr <- newXMLHttpRequest

    resultVar <- newEmptyMVar

    _ <- onAsync xhr load $ liftIO $ do
        result <- mkResult xhr
        putMVar resultVar result

    _ <- onAsync xhr abortEvent $ liftIO $
        putMVar resultVar Error

    _ <- onAsync xhr error $ liftIO $
        putMVar resultVar Error

    openSimple xhr (show method) url
    -- "/posts?limit=10"

    mapM_ (\(k, v) -> setRequestHeader xhr k v) headers

    let p = payload >>= Just . textToJSString . decodeUtf8 . toStrict . encode

    send xhr p
    return (abort xhr, resultVar)
