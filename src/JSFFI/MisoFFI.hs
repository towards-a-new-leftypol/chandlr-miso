{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module JSFFI.MisoFFI
    ( Document (..)
    , Element (..)
    , ParentNode (..)
    , getDocument
    , querySelector
    , getAttribute
    , textContent
    , encodeURIComponent
    , freezeBodyScrolling
    , unFreezeBodyScrolling
    ) where

import Miso.DSL
    ( JSVal
    , jsg
    , jsg1
    , (#)
    , (!)
    , setField
    , fromJSVal
    , fromJSValUnchecked
    , isNull
    , isUndefined
    )
import Miso.String (MisoString, fromMisoString)
import Control.Monad (void)
import qualified Data.Text as T

-- | Newtypes wrapping JSVal for type safety
newtype Document   = Document   JSVal
newtype Element    = Element    JSVal
newtype ParentNode = ParentNode JSVal

-- | Safely checks if a JSVal is null or undefined
maybeNullOrUndefined :: JSVal -> IO (Maybe JSVal)
maybeNullOrUndefined x = do
    nullYes <- isNull x
    if nullYes
        then return Nothing
        else do
            undefYes <- isUndefined x
            if undefYes then return Nothing else return (Just x)

-- | Retrieve the global document object
getDocument :: IO Document
getDocument = Document <$> jsg "document"

-- | Query a descendant element using a CSS selector
querySelector :: ParentNode -> MisoString -> IO (Maybe Element)
querySelector (ParentNode n) s =
    (Element <$>) <$> ((n # "querySelector" $ [s]) >>= maybeNullOrUndefined)

-- | Get an attribute value from an element, converting to MisoString
getAttribute :: JSVal -> MisoString -> IO (Maybe MisoString)
getAttribute x attr = do
    mVal <- (x # "getAttribute" $ [attr]) >>= maybeNullOrUndefined
    case mVal of
        Nothing -> return Nothing
        Just v  -> fromJSVal v >>= \case
            Just s  -> return (Just s)
            Nothing -> fail $ "Attribute '" ++ T.unpack (fromMisoString attr) ++ "' returned non-string value"

-- | Get the text content of an element
textContent :: Element -> IO (Maybe MisoString)
textContent (Element e) = e ! "textContent" >>= fromJSVal

-- | URL-encode a string
encodeURIComponent :: MisoString -> IO MisoString
encodeURIComponent s = jsg1 "encodeURIComponent" s >>= fromJSValUnchecked

-- | Disable scrolling on the body
freezeBodyScrolling :: IO ()
freezeBodyScrolling = do
    doc   <- jsg "document"
    body  <- doc ! "body"
    style <- body ! "style"
    setField style "overflow" "hidden"

-- | Re-enable scrolling on the body
unFreezeBodyScrolling :: IO ()
unFreezeBodyScrolling = do
    doc   <- jsg "document"
    body  <- doc ! "body"
    style <- body ! "style"
    setField style "overflow" "" 
