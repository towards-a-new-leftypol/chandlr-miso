{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Miso
    ( consoleLog
    )
import Miso.JSON (eitherDecode)
import Miso.String
    ( MisoString
    , toMisoString
    , fromMisoString
    )
import Data.Time.Clock (getCurrentTime)
import JSFFI.MisoFFI
    ( getDocument
    , Element (..)
    , Document (..)
    , ParentNode (..)
    , querySelector
    , textContent
    , getAttribute
    , getCookie
    )
import qualified Data.ByteString.Base64 as B64
import Data.Maybe (fromMaybe)
import Data.Bifunctor (first)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set

import Common.FrontEnd.Types
import Common.FrontEnd.JSONSettings
import Common.BitField (intsFromBitField)

getScriptContents :: MisoString -> IO (Maybe MisoString)
getScriptContents className = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "." <> (fromMisoString className)

    case mElem of
        Nothing -> return Nothing
        Just e -> (toMisoString <$>) <$> textContent e


getInitialDataPayload :: IO InitialDataPayload
getInitialDataPayload = do
    maybeRawData <- getScriptContents "initial-data"

    let decoded = first toMisoString
                (B64.decode (fromMisoString (fromMaybe "" maybeRawData)))
                >>= eitherDecode . toMisoString

    either
         ( \err -> do
             consoleLog $ "!!!! Could not parse initial data! Falling back to default values. Error: " <> toMisoString err
             t <- getCurrentTime
             return $ InitialDataPayload t Nil []
         )
         ( \json -> do
            consoleLog "Successfully loaded base64 encoded JSON data from page"
            return json
        )
        decoded


getMetadata :: MisoString -> IO (Maybe MisoString)
getMetadata key = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "meta[name='" <> fromMisoString key <> "']"

    case mElem of
        Nothing -> return Nothing
        Just (Element el) ->
            (toMisoString <$>) <$> getAttribute el ("content" :: MisoString)


getMediaRoot :: IO MisoString
getMediaRoot = getMetadata "media-root" >>=
    return . maybe "undefined" id


settingsFromHtml :: IO JSONSettings
settingsFromHtml = do
    postgrestUrl <- getMetadata "postgrest-url" >>=
        return . maybe "http://localhost:3000" id

    liftIO $ consoleLog $ "postgrest-url " <> postgrestUrl

    postgrestFetchCount <- getMetadata "postgrest-fetch-count" >>=
        return . maybe 1000 fromMisoString

    mediaRoot <- getMediaRoot

    isAdmin <- (Just "True" ==) <$> getMetadata "admin"

    liftIO $ consoleLog $ "media_root: " <> mediaRoot

    return JSONSettings
        { postgrest_url = postgrestUrl
        , jwt = ""
        , postgrest_fetch_count = postgrestFetchCount
        , media_root = mediaRoot
        , media_root_path = ""
        , static_serve_path = ""
        , static_serve_url_root = ""
        , admin = isAdmin
        , spam_noticer_url = ""
        }


getSelectedBoardIdsFromCookie :: IO (Maybe (Set.Set Int))
getSelectedBoardIdsFromCookie = do
    consoleLog "getSelectedBoardIdsFromCookie"
    bCookie <- getCookie boardsSelCookieName

    case bCookie of
        Nothing -> do
            consoleLog "NavigationBar didn't find a b cookie"
            return Nothing
        Just b -> do
            consoleLog $ "NavigationBar b cookie value: " <> b
            return $ Just $ getBoardIdsFromMisoString b

    where
        getBoardIdsFromMisoString :: MisoString -> Set.Set Int
        getBoardIdsFromMisoString = intsFromBitField . read . fromMisoString


boardsSelCookieName :: MisoString
boardsSelCookieName = "b"

