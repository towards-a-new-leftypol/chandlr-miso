{-# LANGUAGE ScopedTypeVariables #-}

module Network.Http
    ( http
    , HttpActionResult
    , HttpMethod (..)
    , HttpResult (..)
    )
where

import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import GHCJS.DOM.XMLHttpRequest
    ( newXMLHttpRequest
    , openSimple
    , getStatus
    , getStatusText
    , getResponseText
    , abort
    -- , send
    )
import GHCJS.DOM.JSFFI.Generated.XMLHttpRequest (send)
import GHCJS.DOM.Types (XMLHttpRequest)
import Data.JSString.Text (textToJSString)
import GHCJS.DOM.EventM (onAsync)
import GHCJS.DOM.XMLHttpRequestEventTarget (load)

-- What we actually want is to call send and not block the thread
--   - so that we can put the request into our list of ongoing requests.
--   - and have the response come back as an Action?
--      - we would need the onload event to send an Action to updateModel

{-
 - Okay what the hell do I even want from an http function?
 -
 - the js implementation has these features:
 -
 -  - url
 -  - method
 -  - ability to abort
 -  - is async
 -  - get the result from it
 -      - this should be a fn (IO result), let's not expose the xhr object?
 -
 -  Also want:
 -  - return the same data structure always, but that data structure should
 -  inform of any errors, and have the status code, response body,
 -  and status text.
 -
 -
 - Do we really need some sort of isomorphic framework? I think that's
 - more work than just using a simple http lib server-side... tbqh
 -}

data HttpMethod = GET | PUT | POST | DELETE | PATCH
    deriving Show

data HttpResult a
    = Error
    | HttpResponse
        { status_code :: Int
        , status_text :: String
        , body        :: Maybe a
        }

type HttpActionResult = (IO (), MVar (HttpResult Text)) -- (abort, result)


mkResult :: XMLHttpRequest -> IO (HttpResult Text)
mkResult xhr = do
        sc <- getStatus xhr

        let status_code_int :: Int = fromEnum sc

        st :: String <- getStatusText xhr

        mBody :: Maybe Text <- getResponseText xhr

        return HttpResponse
            { status_code = status_code_int
            , status_text = st
            , body = mBody
            }


http
    :: String
    -> HttpMethod
    -> Maybe Text
    -> IO HttpActionResult
http url method payload = do
    xhr <- newXMLHttpRequest

    resultVar <- newEmptyMVar

    _ <- onAsync xhr load $ liftIO $ do
        result <- mkResult xhr
        putMVar resultVar result

    openSimple xhr (show method) url
    -- "/posts?limit=10"

    send xhr (payload >>= Just . textToJSString)
    return (abort xhr, resultVar)
