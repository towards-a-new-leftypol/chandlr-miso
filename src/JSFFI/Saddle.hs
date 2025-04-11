{-# LANGUAGE OverloadedStrings #-}

module JSFFI.Saddle
    ( Document (..)
    , getDocument
    , querySelector
    , Element (..)
    , ParentNode (..)
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
getDocument = Document <$> jsg "document"

querySelector :: ParentNode -> JSString -> JSM (Maybe JSVal)
querySelector (ParentNode n) s =
    (n # "querySelector" $ [s]) >>= maybeNullOrUndefined
