{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Data.Aeson (eitherDecodeStrict)
import Miso
    ( consoleLog
    , View
    , URI
    )
import Miso.String
    ( MisoString
    , toMisoString
    , MisoString
    , toMisoString
    , fromMisoString
    )
import Servant.Miso.Router (route)
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
import Data.Proxy (Proxy (..))
import Servant.API hiding (URI)
import Data.Either (fromRight)

import Common.FrontEnd.Types
import Common.FrontEnd.Routes (Route)
import Common.FrontEnd.Model (Model)
import Common.FrontEnd.Action (Action)

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

    let rawData = (B64.decode $ fromMisoString (fromMaybe "" maybeRawData)) >>= eitherDecodeStrict

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
        rawData


data PageType = Catalog | Search | Thread
    deriving Eq


pageTypeFromURI :: URI -> PageType
pageTypeFromURI = do
    -- default to Catalog in case of routing error.
    fromRight Catalog . routeResult

    where
        routeResult uri = route (Proxy :: Proxy (Route (View Model Action))) handlers (const uri) undefined

        handlers = hLatest :<|> hThread :<|> hSearch

        hLatest :: m -> PageType
        hLatest = const Catalog

        hThread :: a -> a -> b -> m -> PageType
        hThread = const $ const $ const $ const Thread

        hSearch :: Maybe a -> m -> PageType
        hSearch = const $ const Search


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
