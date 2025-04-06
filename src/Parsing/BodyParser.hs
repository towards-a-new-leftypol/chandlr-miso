{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Parsing.BodyParser
    ( PostPart (..)
    , parsePostBody
    , collectBacklinks
    , Backlinks
    ) where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Miso (consoleLog)

import JSFFI.FFI
    ( js_document
    , js_createElement
    , js_setInnerHTML
    , js_getChildNodes
    , js_getClassList
    , js_contains
    , js_getAttribute
    , js_getTagName
    , js_getNodeType
    , js_getTextContent
    , js_getNodeListItem
    , js_getNodeListLength
    , Element
    , JSString
    , Node
    , NodeList
    , NodeType (..)
    , elemToNode
    , nodeToElem
    , fromJSString
    )
import Common.Parsing.PostPartType
import Common.Parsing.QuoteLinkParser
import Common.Parsing.PostBodyUtils


nodeListToList :: NodeList -> IO [ Node ]
nodeListToList l = do
  len <- js_getNodeListLength l
  nodes <- mapM (js_getNodeListItem l) [0..len-1]
  return $ catMaybes nodes


-- | Parse the HTML string and add event handlers to certain elements
parsePostBody :: Text -> IO [ PostPart ]
parsePostBody htmlString = do
  Just doc <- js_document

  container <- js_createElement doc "div"

  -- Set the innerHTML of the container to the HTML string
  js_setInnerHTML container htmlString -- ERROR: htmlString is the wrong type here! (why doesn't the compiler tell us?)

  children <- js_getChildNodes (elemToNode container)

  parseNodeList children


toPostPart :: Node -> IO PostPart
toPostPart node = do
    node_type <- js_getNodeType node

    toPostPart_ node_type node


toPostPart_ :: Word -> Node -> IO PostPart
toPostPart_ node_type node
    | node_type == TEXT_NODE =
        js_getTextContent node >>= return . SimpleText
    | node_type == ELEMENT_NODE = do
        tagName :: JSString <- js_getTagName element

        case tagName of
          "A"      -> parseAnchor element
          "SPAN"   -> parseSpan element
          "EM"     -> parseEm element
          "STRONG" -> parseStrong element
          "U"      -> parseU element
          "S"      -> parseS element
          "PRE"    -> parseCode element
          "BR"     -> return Skip
          _        -> do
            consoleLog tagName
            return $ SimpleText "Unsupported element"
    | otherwise = return Skip

    where
        element = nodeToElem node


parseAnchor :: Element -> IO PostPart
parseAnchor element = do
  m_href :: Maybe JSString <- js_getAttribute element ("href" :: JSString)

  case m_href of
    Nothing -> return $ SimpleText "Anchor without href"
    Just href -> do
      target <- js_getAttribute element ("target" :: JSString)

      case target of
        Just ("_blank" :: JSString) -> return $ PostedUrl href
        _ -> return $ Quote $ parseURL $ fromJSString href


parseSpan :: Element -> IO PostPart
parseSpan element = do
  classList <- js_getClassList element

  quote       <- js_contains classList ("quote" :: JSString)
  orangeQuote <- js_contains classList ("orangeQuote" :: JSString)
  heading     <- js_contains classList ("heading" :: JSString)
  spoiler     <- js_contains classList ("spoiler" :: JSString)

  if | quote       -> parseChildNodes element >>= return . GreenText
     | orangeQuote -> parseChildNodes element >>= return . OrangeText
     | heading     -> parseChildNodes element >>= return . RedText
     | spoiler     -> parseChildNodes element >>= return . Spoiler
     | otherwise   -> return $ SimpleText "Unsupported span class"


parseNodeList :: NodeList -> IO [ PostPart ]
parseNodeList nodes = nodeListToList nodes >>= mapM toPostPart

parseChildNodes :: Element -> IO [ PostPart ]
parseChildNodes element = js_getChildNodes (elemToNode element) >>= parseNodeList

parseEm :: Element -> IO PostPart
parseEm element
    = parseChildNodes element
    >>= return . Italics

parseStrong :: Element -> IO PostPart
parseStrong element
    = parseChildNodes element
    >>= return . Bold

parseU :: Element -> IO PostPart
parseU element
    = parseChildNodes element
    >>= return . Underlined

parseS :: Element -> IO PostPart
parseS element
    = parseChildNodes element
    >>= return . Strikethrough

parseCode :: Element -> IO PostPart
parseCode element
    = parseChildNodes element
    >>= return . Code
