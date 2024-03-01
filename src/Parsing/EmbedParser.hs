module Parsing.EmbedParser
    ( extractVideoId
    )
    where

import Text.Parsec
import Text.Parsec.String

-- Parser to extract the video ID
videoIdParser :: Parser String
videoIdParser = do
  -- Look for the data-video attribute
  _ <- manyTill anyChar (try (string "data-video=\"") <|> string "href=\"https://youtu.be/")
  -- Capture the video ID
  videoId <- manyTill anyChar (try (char '\"') <|> (char '"' >> char ' '))
  -- Return the captured ID
  return videoId

-- Function to apply the parser and extract the video ID
extractVideoId :: String -> Either ParseError String
extractVideoId input = parse videoIdParser "" input
