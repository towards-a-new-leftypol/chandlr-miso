{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Miso
    ( consoleLog
    , run
    , miso
    , getURI
    )
import Miso.String (fromMisoString)
import Language.Javascript.JSaddle.Monad (JSM)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Control.Monad.IO.Class (liftIO)

import Common.FrontEnd.MainComponent
import Common.FrontEnd.JSONSettings (fromHtml)
import Common.FrontEnd.Types (InitialDataPayload (InitialDataPayload))
import Utils (getMetadata)

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif


main :: IO ()
main = run mainMain

mainMain :: JSM ()
mainMain = do
    consoleLog "Haskell begin."

    jsonSettings <- fromHtml

    uri <- getURI

    currentTime <- getMetadata "timestamp"
        >>= maybe (liftIO getCurrentTime) (iso8601ParseM . fromMisoString)

    miso $ const $ app jsonSettings uri $
        InitialDataPayload currentTime undefined
