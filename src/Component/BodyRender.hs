{-# LANGUAGE OverloadedStrings #-}

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
    )

import BodyParser (PostPart (..))

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
render :: [ PostPart ] -> [ View a ]
render = map renderPostPart

renderPostPart :: PostPart -> View a
renderPostPart (SimpleText txt) = text txt
renderPostPart (PostedUrl u) =
    a_
        [ href_ u
        , target_ "_blank"
        ]
        [ text u ]
renderPostPart Skip = br_ []
renderPostPart (Quote url) =
    a_
        [ href_ url ]
        [ text url ]

renderPostPart (GreenText parts) =
    span_ [ class_ "quote" ] (render parts)

renderPostPart (OrangeText parts) =
    span_ [ class_ "orangeQuote" ] (render parts)

renderPostPart (RedText parts) =
    span_ [ class_ "heading" ] (render parts)

renderPostPart (Spoiler parts) =
    span_ [ class_ "spoiler" ] (render parts)

renderPostPart (Bold parts) =
    strong_ [] (render parts)

renderPostPart (Underlined parts) =
    u_ [] (render parts)

renderPostPart (Italics parts) =
    em_ [] (render parts)

renderPostPart (Strikethrough parts) =
    s_ [] (render parts)

