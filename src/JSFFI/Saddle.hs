module JSFFI.Saddle
    ( Document (..)
    , getDocument
    ) where

import Language.Javascript.JSaddle
    ( JSM
    , JSVal
    , jsg
    )

newtype Document = Document JSVal

getDocument :: JSM Document
getDocument = Document <$> jsg "document"
