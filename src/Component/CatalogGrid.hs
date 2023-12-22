{-# LANGUAGE OverloadedStrings #-}

module Component.CatalogGrid
( Model
, initialModel
, Action
, Interface (..)
, view
) where

import Miso
    ( View
    , div_
    , class_
    , img_
    , href_
    , a_
    , src_
    , alt_
    , title_
    , strong_
    , span_
    , p_
    , br_
    , id_
    )

type Model = ()

initialModel :: Model
initialModel = ()

type Action = ()

data Interface a = Interface
    { passAction   :: Action -> a
    , selectThread :: ()
    }

view :: Interface a -> Model -> View a
view iface model =
    div_
        [ class_ "threads" ]
        [ div_
            [ id_ "Grid" ]
            [ gridItem | _ <- [0..10] ]
        ]

gridItem :: View a
gridItem =
    div_
        [ class_ "mix" ]
        [ div_
            [ class_ "thread grid-li grid-size-small" ]
            [ a_
                [ href_ "/a/res/1.html" ]
                [ img_
                    [ class_ "thread-image"
                    , src_ "/a/thumb/1111111111111.png"
                    , alt_ "Opening post image"
                    , title_ "Dec 18 23:12"
                    ]
                ]
            , div_
                [ class_ "replies" ]
                [ strong_ [][ "R: 517 / I: 204" ]
                , p_
                    [ class_ "intro" ]
                    [ span_
                        [ class_ "subject" ][ "This is the thread subject, usually a brief description." ]
                    ]
                , "Hello World"
                , br_ []
                , "Hello World2"
                ]
            ]
        ]
