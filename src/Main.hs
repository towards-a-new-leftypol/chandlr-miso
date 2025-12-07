{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Miso
    ( consoleLog
    , run
    , miso
    , getURI
    , toMisoString
    )
import Language.Javascript.JSaddle.Monad (JSM)
import Control.Monad.IO.Class (liftIO)

import Common.FrontEnd.MainComponent
import Common.FrontEnd.Types
import Utils (settingsFromHtml, getInitialDataPayload)
import Data.IORef (newIORef)

import JSFFI.Profile (bracket, sectionStart, toJSString)

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif


main :: IO ()
main = run mainMain

mainMain :: JSM ()
mainMain = do
    consoleLog "Haskell begin."

    liftIO $ sectionStart $ toJSString "pageLoad"
    ctxRef <- bracket "mainInit" $ do
        jsonSettings <- settingsFromHtml

        consoleLog $ toMisoString $ show jsonSettings

        uri <- getURI

        -- currentTime <- getMetadata "timestamp"
        --     >>= maybe (liftIO getCurrentTime) (iso8601ParseM . fromMisoString)

        ctx <- AppInitCtx uri jsonSettings <$> getInitialDataPayload

        liftIO $ newIORef ctx

    miso $ const $ app ctxRef
