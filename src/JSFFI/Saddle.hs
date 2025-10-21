{-# LANGUAGE OverloadedStrings #-}

module JSFFI.Saddle
    ( Document (..)
    , Element (..)
    , ParentNode (..)
    , getDocument
    , querySelector
    , getAttribute
    , textContent
    , encodeURIComponent
    , freezeBodyScrolling
    ) where

import Language.Javascript.JSaddle
    ( JSM
    , JSVal
    , JSString
    , jsg
    , jsg1
    , js
    , (#)
    , maybeNullOrUndefined
    , val
    )

import Control.Monad (void)
import Control.Lens.Operators ((^.))
import Data.Text as T
import Language.Javascript.JSaddle.String (textFromJSString)
import Language.Javascript.JSaddle.Classes (fromJSVal, fromJSValUnchecked)

newtype Document = Document JSVal
newtype Element = Element JSVal
newtype ParentNode = ParentNode JSVal

getDocument :: JSM Document
getDocument = Document <$> jsg ("document" :: JSString)

querySelector :: ParentNode -> JSString -> JSM (Maybe Element)
querySelector (ParentNode n) s =
    (Element <$>) <$> ((n # ("querySelector" :: JSString) $ [s]) >>= maybeNullOrUndefined)

getAttribute1 :: JSVal -> JSString -> JSM (Maybe JSVal)
getAttribute1 x attr =
    (x # ("getAttribute" :: JSString) $ [attr]) >>= maybeNullOrUndefined


textContent :: Element -> JSM (Maybe JSString)
textContent (Element e) = (e ^. js ("textContent" :: JSString)) >>= fromJSVal


-- Modified version returning Maybe JSString
getAttribute :: JSVal -> JSString -> JSM (Maybe JSString)
getAttribute x attr =
    getAttribute1 x attr >>= traverse convertToJSString

    where
        convertToJSString :: JSVal -> JSM JSString
        convertToJSString val = do
            mStr <- fromJSVal val  -- Direct conversion
            case mStr of
              Just s  -> return s
              Nothing -> do
                  -- Pure conversion for error message
                  let attrText = textFromJSString attr
                  fail $ "Attribute '" ++ T.unpack attrText ++ "' returned non-string value"


encodeURIComponent :: JSString -> JSM JSString
encodeURIComponent s = jsg1 ("encodeURIComponent" :: JSString) s >>= fromJSValUnchecked

-- decode base64
-- aToB :: JSString -> JSM (Maybe JSString)
-- aToB s = jsg1 ("atob" :: JSString) s >>= fromJSVal

-- freezeBodyScrolling :: JSM ()
-- freezeBodyScrolling = do
--   Document doc <- getDocument
--   body <- doc # ("body" :: JSString) $ []
--   style <- body # ("style" :: JSString) $ []
--   style # ("overflow" :: JSString) $ [ val ("hidden" :: JSString) ]

-- freezeBodyScrolling :: JSM ()
-- freezeBodyScrolling = do
--   Document doc <- getDocument
--   body  <- (doc # ("body" :: JSString) $ []) :: JSM JSVal
--   style <- (body # ("style" :: JSString) $ []) :: JSM JSVal
--   void $ style # ("overflow" :: JSString) $ [val ("hidden" :: JSString)]

freezeBodyScrolling :: JSM ()
freezeBodyScrolling = do
  Document doc <- getDocument
  body  <- doc # ("body" :: JSString) $ ([] :: [JSVal])
  style <- body # ("style" :: JSString) $ ([] :: [JSVal])
  void $ style # ("overflow" :: JSString) $ [val ("hidden" :: JSString)]
