module JSFFI.FFI
( JSString
, Document
, Element
, Node
, NodeList
, DomTokenList
, NodeType (..)
, js_document
, js_createElement
, js_setInnerHTML
, js_getChildNodes
, js_getNodeListLength
, js_getNodeListItem
, js_getClassList
, js_contains
, js_getAttribute
, js_getTagName
, js_getNodeType
, js_getTextContent
, elemToNode
, nodeToElem
, fromJSString
) where

import GHC.Wasm.Prim (JSVal, JSString(..), fromJSString)

newtype Document = Document JSVal
newtype Element = Element JSVal
newtype Node = Node JSVal
newtype NodeList = NodeList JSVal
newtype DomTokenList = DomTokenList JSVal

data NodeType
    = ELEMENT_NODE
    | ATTRIBUTE_NODE
    | TEXT_NODE
    | CDATA_SECTION_NODE
    | ENTITY_REFERENCE_NODE -- deprecated
    | ENTITY_NODE -- deprecated
    | PROCESSING_INSTRUCTION_NODE
    | COMMENT_NODE
    | DOCUMENT_NODE
    | DOCUMENT_TYPE_NODE
    | DOCUMENT_FRAGMENT_NODE
    | NOTATION_NODE -- deprecated
    deriving Eq

nodeTypeFromInt :: Int -> NodeType
nodeTypeFromInt 1 = ELEMENT_NODE
nodeTypeFromInt 2 = ATTRIBUTE_NODE
nodeTypeFromInt 3 = TEXT_NODE
nodeTypeFromInt 4 = CDATA_SECTION_NODE
nodeTypeFromInt 5 = ENTITY_REFERENCE_NODE -- deprecated
nodeTypeFromInt 6 = ENTITY_NODE -- deprecated
nodeTypeFromInt 7 = PROCESSING_INSTRUCTION_NODE
nodeTypeFromInt 8 = COMMENT_NODE
nodeTypeFromInt 9 = DOCUMENT_NODE
nodeTypeFromInt 10 = DOCUMENT_TYPE_NODE
nodeTypeFromInt 11 = DOCUMENT_FRAGMENT_NODE
nodeTypeFromInt 12 = NOTATION_NODE -- deprecated
nodeTypeFromInt _ = undefined

foreign import javascript unsafe "document"
    js_document :: IO Document

foreign import javascript unsafe "$1.createElement($2)"
    js_createElement :: Document -> JSString -> IO Element

foreign import javascript unsafe "$1.innerHTML = $2"
    js_setInnerHTML :: Element -> JSString -> IO ()

foreign import javascript unsafe "$1.childNodes"
    js_getChildNodes :: Node -> IO NodeList

foreign import javascript unsafe "$1.length"
    js_getNodeListLength :: NodeList -> IO Int

foreign import javascript unsafe "$1.item($2)"
    js_getNodeListItem :: NodeList -> Int -> IO Node

foreign import javascript unsafe "$1.classList"
    js_getClassList :: Element -> IO DomTokenList

foreign import javascript unsafe "$1.contains($2)"
    js_contains :: DomTokenList -> JSString -> IO Bool

foreign import javascript unsafe "$1.getAttribute($2)"
    js_getAttribute :: Element -> JSString -> IO JSString

foreign import javascript unsafe "$1.tagName"
    js_getTagName :: Element -> IO JSString

foreign import javascript unsafe "$1.nodeType"
    js_getNodeType_ :: Node -> IO Int

js_getNodeType :: Node -> IO NodeType
js_getNodeType n = js_getNodeType_ n >>= return . nodeTypeFromInt

foreign import javascript unsafe "$1.textContent"
    js_getTextContent :: Node -> IO JSString

elemToNode :: Element -> Node
elemToNode (Element x) = Node x

nodeToElem :: Node -> Element
nodeToElem (Node x) = Element x
