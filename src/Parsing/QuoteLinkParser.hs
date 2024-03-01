module Parsing.QuoteLinkParser
    ( parseURL
    , ParsedURL (..)
    )
    where

import Text.Parsec
import Text.Parsec.String (Parser)

-- Define a data type to hold the extracted components
data ParsedURL = ParsedURL
  { boardName   :: String
  , threadId    :: Maybe Integer
  , postId      :: Maybe Integer
  } deriving (Show, Eq)

-- Parser for a segment of the path
segment :: Parser String
segment = many (noneOf "/#")

-- Parser for an integer
integer :: Parser Integer
integer = read <$> many1 digit

-- Parser for the board name
boardNameParser :: Parser String
boardNameParser = char '/' >> segment

-- Optional parser for the thread number
threadNumberParser :: Parser (Maybe Integer)
threadNumberParser = optionMaybe $ try $ do
    _ <- char '/' >> string "res/"
    tId <- integer
    _ <- string ".html"
    return tId

-- Parser for index.html, returning Nothing for threadId and postId
indexParser :: Parser (Maybe Integer, Maybe Integer)
indexParser = try $ do
  _ <- string "/index.html"
  return (Nothing, Nothing)

-- Combined URL parser
urlParser :: Parser ParsedURL
urlParser = do
    bName <- boardNameParser

    (tId, pId) <- try threadNumberParser >>= \mTid ->
      case mTid of
          Just tId -> do
              pId <- postIdParser
              return (Just tId, pId)
          Nothing -> indexParser
    eof  -- Expect the end of input
    return $ ParsedURL bName tId pId

-- Optional parser for the post ID
postIdParser :: Parser (Maybe Integer)
postIdParser = optionMaybe $ char '#' >> integer

-- Function to run the parser
parseURL :: String -> Either ParseError ParsedURL
parseURL = parse urlParser ""
