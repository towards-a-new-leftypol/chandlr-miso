{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component.BodyRender where

import Miso
    ( text
    , href_
    , a_
    , View
    , target_
    , br_
    , span_
    , class_
    , strong_
    , u_
    , em_
    , s_
    , title_
    )

import Miso.String (toMisoString, fromMisoString)
import Text.Parsec (ParseError)
import BodyParser (PostPart (..))
import QuoteLinkParser
import qualified Component.Thread.Model as Model
import qualified Network.SiteType as Site
import qualified Network.ThreadType as Thread

{-
 - This is the inverse of parsePostBody from BodyParser except
 - that the output is a miso View and not Text. It might be
 - worth trying to use quickcheck to verify the bijection between
 - Text and DOM by creating random PostParts and seeing if 
 - ((parsePostBody . render) parts == parts)
 -
 - (is there an easy way to render a miso View?, that's what's missing
 - a f :: View a -> Text)
 -}
render :: Model.Model -> [ PostPart ] -> [ View a ]
render m = map (renderPostPart m)

renderPostPart :: Model.Model -> PostPart -> View a
renderPostPart _ (SimpleText txt) = text txt
renderPostPart _ (PostedUrl u) =
    a_
        [ href_ u
        , target_ "_blank"
        ]
        [ text u ]

renderPostPart _ Skip = br_ []

renderPostPart m (Quote url) = elems parse_result
    where
        parse_result = parseURL $ fromMisoString url

        elems :: Either ParseError ParsedURL -> View a
        elems (Left err) =
            a_
                [ href_ url
                , title_ $ toMisoString $ show err
                ]
                [ text url ]
        elems (Right ParsedURL {..}) =
            a_
                [ href_ url ]
                [ text url ]

renderPostPart m (GreenText parts) =
    span_ [ class_ "quote" ] (render m parts)

renderPostPart m (OrangeText parts) =
    span_ [ class_ "orangeQuote" ] (render m parts)

renderPostPart m (RedText parts) =
    span_ [ class_ "heading" ] (render m parts)

renderPostPart m (Spoiler parts) =
    span_ [ class_ "spoiler" ] (render m parts)

renderPostPart m (Bold parts) =
    strong_ [] (render m parts)

renderPostPart m (Underlined parts) =
    u_ [] (render m parts)

renderPostPart m (Italics parts) =
    em_ [] (render m parts)

renderPostPart m (Strikethrough parts) =
    s_ [] (render m parts)

