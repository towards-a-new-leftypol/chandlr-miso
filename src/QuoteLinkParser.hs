module QuoteLinkParser
    ( parseURL
    , ParsedURL (..)
    )
    where

import Text.Parsec
import Text.Parsec.String (Parser)


data ParsedURL = ParsedURL
  { siteName    :: Maybe String
  , boardName   :: String
  , threadId    :: Maybe Integer
  , postId      :: Maybe Integer
  } deriving (Show)


-- Parser for a segment of the path
segment :: Parser String
segment = many (noneOf "/#")

-- Parser for an integer
integer :: Parser Integer
integer = read <$> many1 digit

-- Parser for the site name
siteNameParser :: Parser (Maybe String)
siteNameParser = optionMaybe $ char '/' >> segment

-- Parser for the board name
boardNameParser :: Parser String
boardNameParser = char '/' >> segment

-- Optional parser for the thread number
threadNumberParser :: Parser (Maybe Integer)
threadNumberParser = optionMaybe $ try (char '/' >> string "res/" >> integer)

-- Optional parser for the post ID
postIdParser :: Parser (Maybe Integer)
postIdParser = optionMaybe $ char '#' >> integer

-- Combined URL parser
urlParser :: Parser ParsedURL
urlParser = do
  sName <- siteNameParser
  bName <- boardNameParser
  tId <- threadNumberParser
  pId <- postIdParser
  eof  -- Expect the end of input
  return $ ParsedURL sName bName tId pId

-- Function to run the parser
parseURL :: String -> Either ParseError ParsedURL
parseURL = parse urlParser "chan"
