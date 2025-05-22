{-# LANGUAGE OverloadedStrings #-}

module JSFFI.Saddle
    ( Document (..)
    , Element (..)
    , ParentNode (..)
    , getDocument
    , querySelector
    , getAttribute
    ) where

import Language.Javascript.JSaddle
    ( JSM
    , JSVal
    , JSString
    , jsg
    , (#)
    , maybeNullOrUndefined
    )

newtype Document = Document JSVal
newtype Element = Element JSVal
newtype ParentNode = ParentNode JSVal

getDocument :: JSM Document
getDocument = Document <$> jsg ("document" :: JSString)

querySelector :: ParentNode -> JSString -> JSM (Maybe Element)
querySelector (ParentNode n) s =
    (Element <$>) <$> ((n # ("querySelector" :: JSString) $ [s]) >>= maybeNullOrUndefined)

getAttribute :: JSVal -> JSString -> JSM (Maybe JSVal)
getAttribute x attr =
    (x # ("getAttribute" :: JSString) $ [attr]) >>= maybeNullOrUndefined
