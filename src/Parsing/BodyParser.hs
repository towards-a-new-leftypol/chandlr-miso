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
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types
    ( Element (..)
    , JSString
    , NodeList
    , uncheckedCastTo
    )
import GHCJS.DOM.JSFFI.Generated.Document
import GHCJS.DOM.JSFFI.Generated.Element
import GHCJS.DOM.JSFFI.Generated.Node hiding (contains)
import qualified GHCJS.DOM.JSFFI.Generated.NodeList as NodeList
import GHCJS.DOM.JSFFI.Generated.DOMTokenList (contains)
import Data.Text (Text)
import Miso (consoleLog)
import Miso.String (fromMisoString)

import Common.Parsing.PostPartType
import Common.Parsing.QuoteLinkParser
import Common.Parsing.PostBodyUtils


nodeListToList :: NodeList -> IO [ Node ]
nodeListToList l = do
  len <- NodeList.getLength l
  nodes <- mapM (NodeList.item l) [0..len-1]
  return $ catMaybes nodes


-- | Parse the HTML string and add event handlers to certain elements
parsePostBody :: Text -> IO [ PostPart ]
parsePostBody htmlString = do
  Just doc <- currentDocument

  container <- createElement doc ("div" :: Text)

  -- Set the innerHTML of the container to the HTML string
  setInnerHTML container htmlString

  children <- getChildNodes container

  parseNodeList children


toPostPart :: Node -> IO PostPart
toPostPart node = do
    node_type <- getNodeType node

    toPostPart_ node_type node


toPostPart_ :: Word -> Node -> IO PostPart
toPostPart_ node_type node
    | node_type == TEXT_NODE =
        getTextContentUnchecked node >>= return . SimpleText
    | node_type == ELEMENT_NODE = do
        tagName :: JSString <- getTagName element

        case tagName of
          "A"      -> parseAnchor element
          "SPAN"   -> parseSpan element
          "EM"     -> parseEm element
          "STRONG" -> parseStrong element
          "U"      -> parseU element
          "S"      -> parseS element
          "BR"     -> return Skip
          _        -> do
            consoleLog tagName
            return $ SimpleText "Unsupported element"
    | otherwise = return Skip

    where
        element = uncheckedCastTo Element node



parseAnchor :: Element -> IO PostPart
parseAnchor element = do
  m_href :: Maybe JSString <- getAttribute element ("href" :: JSString)

  case m_href of
    Nothing -> return $ SimpleText "Anchor without href"
    Just href -> do
      target <- getAttribute element ("target" :: JSString)

      case target of
        Just ("_blank" :: JSString) -> return $ PostedUrl href
        _ -> return $ Quote $ parseURL $ fromMisoString href


parseSpan :: Element -> IO PostPart
parseSpan element = do
  classList <- getClassList element

  quote       <- contains classList ("quote" :: JSString)
  orangeQuote <- contains classList ("orangeQuote" :: JSString)
  heading     <- contains classList ("heading" :: JSString)
  spoiler     <- contains classList ("spoiler" :: JSString)

  if | quote       -> parseChildNodes element >>= return . GreenText
     | orangeQuote -> parseChildNodes element >>= return . OrangeText
     | heading     -> parseChildNodes element >>= return . RedText
     | spoiler     -> parseChildNodes element >>= return . Spoiler
     | otherwise   -> return $ SimpleText "Unsupported span class"


parseNodeList :: NodeList -> IO [ PostPart ]
parseNodeList nodes = nodeListToList nodes >>= mapM toPostPart

parseChildNodes :: Element -> IO [ PostPart ]
parseChildNodes element = getChildNodes element >>= parseNodeList

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
