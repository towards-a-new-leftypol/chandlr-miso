{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Data.Aeson (eitherDecodeStrict)
import Miso
    ( consoleLog
    )
import Miso.String
    ( MisoString
    , toMisoString
    , fromMisoString
    )
import Language.Javascript.JSaddle.Monad (JSM)
import Data.Time.Clock (getCurrentTime)
import JSFFI.Saddle
    ( getDocument
    , Element (..)
    , Document (..)
    , ParentNode (..)
    , querySelector
    , textContent
    , getAttribute
    )
import qualified Data.ByteString.Base64 as B64
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)

import Common.FrontEnd.Types
import Common.FrontEnd.JSONSettings

getScriptContents :: MisoString -> JSM (Maybe MisoString)
getScriptContents className = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "." <> (fromMisoString className)

    case mElem of
        Nothing -> return Nothing
        Just e -> (toMisoString <$>) <$> textContent e


getInitialDataPayload :: JSM InitialDataPayload
getInitialDataPayload = do
    maybeRawData <- getScriptContents "initial-data"

    let decoded = B64.decode (fromMisoString (fromMaybe "" maybeRawData)) >>= eitherDecodeStrict

    either
         ( \err -> do
             consoleLog $ "!!!! Could not parse initial data! Falling back to default values. Error: " <> toMisoString err
             t <- liftIO getCurrentTime
             return $ InitialDataPayload t Nil
         )
         ( \json -> do
            consoleLog "Successfully loaded base64 encoded JSON data from page"
            return json
        )
        decoded


getMetadata :: MisoString -> JSM (Maybe MisoString)
getMetadata key = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "meta[name='" <> fromMisoString key <> "']"

    case mElem of
        Nothing -> return Nothing
        Just (Element el) ->
            (toMisoString <$>) <$> getAttribute el ("content" :: MisoString)


getMediaRoot :: JSM MisoString
getMediaRoot = getMetadata "media-root" >>=
    return . maybe "undefined" id


settingsFromHtml :: JSM JSONSettings
settingsFromHtml = do
    postgrestUrl <- getMetadata "postgrest-url" >>=
        return . maybe "http://localhost:3000" id
    consoleLog $ "postgrest-url " <> postgrestUrl

    postgrestFetchCount <- getMetadata "postgrest-fetch-count" >>=
        return . maybe 1000 fromMisoString

    mediaRoot <- getMediaRoot

    isAdmin <- (Just "True" ==) <$> getMetadata "admin"

    consoleLog $ "media_root: " <> mediaRoot

    return JSONSettings
        { postgrest_url = postgrestUrl
        , jwt = ""
        , postgrest_fetch_count = postgrestFetchCount
        , media_root = mediaRoot
        , media_root_path = ""
        , static_serve_path = ""
        , static_serve_url_root = ""
        , admin = isAdmin
        }
