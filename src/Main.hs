{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Miso
    ( consoleLog
    , run
    , miso
    , startApp
    , getURI
    , toMisoString
    , fromMisoString
    )
import Language.Javascript.JSaddle.Monad (JSM)
import Control.Monad.IO.Class (liftIO)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)

import Common.FrontEnd.MainComponent
import Common.FrontEnd.Types hiding (hydrate)
import Utils (settingsFromHtml, getInitialDataPayload, getMetadata)
import Data.IORef (newIORef)

import JSFFI.Profile (sectionStart, toJSString)

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif


main :: IO ()
main = run mainMain

mainMain :: JSM ()
mainMain = do
    consoleLog "Haskell begin."

    liftIO $ sectionStart $ toJSString "pageLoad"

    jsonSettings <- settingsFromHtml

    consoleLog $ toMisoString $ show jsonSettings

    hydrateStr <- getMetadata "hydrate"

    let hydrate = fromMaybe False (readMaybe =<< (return . fromMisoString) =<< hydrateStr)

    uri <- getURI

    -- currentTime <- getMetadata "timestamp"
    --     >>= maybe (liftIO getCurrentTime) (iso8601ParseM . fromMisoString)

    ctx <-
        if hydrate
        then
            AppInitCtx True uri jsonSettings <$> getInitialDataPayload
        else do
            now <- liftIO getCurrentTime
            consoleLog $ "hydrate is off, current time: " <> toMisoString (show now)
            return $ AppInitCtx False uri jsonSettings (InitialDataPayload now Nil)

    ctxRef <- liftIO $ newIORef ctx

    if hydrate
    then
        miso $ const $ app ctxRef
    else
        startApp $ app ctxRef
