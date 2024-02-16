module PostPartType where

import GHCJS.DOM.Types (JSString)
import Text.Parsec (ParseError)

import QuoteLinkParser (ParsedURL)

data PostPart
    = SimpleText JSString
    | PostedUrl JSString
    | Skip
    | Quote (Either ParseError ParsedURL)
        -- Quotes don't seem to be able to be spoilered
        -- board links (which appear as quotes but start with >>>) break the tag
    | GreenText     [ PostPart ]
    | OrangeText    [ PostPart ]
    | RedText       [ PostPart ]
    | Spoiler       [ PostPart ]
    -- you can't seem to spoiler greentext
    | Bold          [ PostPart ]
    | Underlined    [ PostPart ]
    | Italics       [ PostPart ]
    | Strikethrough [ PostPart ]
    deriving (Show, Eq)


