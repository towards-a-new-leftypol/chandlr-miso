{-# LANGUAGE OverloadedStrings #-}

module Component.ThreadView
( Model (..)
, initialModel
, Action (..)
, update
, view
) where

import Miso
  ( View
  , Effect
  , div_
  , text
  , h1_
  , noEff
  , href_
  , a_
  , class_
  , id_
  , textProp
  , title_
  , alt_
  , src_
  , style_
  , img_
  , span_
  , time_
  , h2_
  , rawHtml
  , loading_
  , download_
  , small_
  , p_
  , for_
  , label_
  )

import Data.Maybe (maybeToList)
import qualified Data.Map as Map
import Miso.String (toMisoString)
import GHCJS.DOM.Types (JSString)

import Network.SiteType (Site)
import qualified Network.SiteType as Site
import Network.PostType (Post)
import qualified Network.PostType as Post
import qualified Network.BoardType as Board
import qualified Network.ThreadType as Thread

data Model = Model
  { site :: Site
  , media_root :: JSString
  } deriving Eq

initialModel :: JSString -> Site -> Model
initialModel mroot s = Model
    { site = s
    , media_root = mroot
    }

data Action = RenderSite Site

update :: Action -> Model -> Effect a Model
update (RenderSite s) m = noEff (m { site = s })

view :: Model -> View a
view m =
  div_
    []
    (
        [ h1_ [] [ text $ toMisoString $ Site.name $ site m ]
        , op_post thread_posts
        ]
        ++ map reply (drop 1 thread_posts)
    )

    where
        thread_posts :: [ Post ]
        thread_posts =
            concatMap (Thread.posts) $
                concatMap (Board.threads) $
                    Site.boards (site m)

        op_post :: [ Post ] -> View a
        op_post [] = h2_ [] [ "There's nothing here" ]
        op_post (x:_) = op x


body :: Post -> [ View a ]
body post = map (rawHtml . toMisoString) $ maybeToList $ Post.body post

op :: Post -> View a
op op_post = div_
    [ class_ "post op"
    , id_ "op_477700"
    ]
    [ span_
        [ class_ "intro" ]
            [ span_
                [ class_ "subject" ][ "Israel 'at war' as Hamas gunmen launch surprise attack from Gaza Thread Pt. II" ]
            , span_
                [ class_ "name" ][ "Anonymous" ]
            , img_
                [ style_ $ Map.fromList
                    [
                        ( "max-height"
                        , "16px"
                        )
                    ,
                        ( "width"
                        , "auto"
                        )
                    ]
                , class_ "flag"
                , src_ "/static/flags/acceleration.png"
                , alt_ "Acceleration"
                , title_ "Acceleration"
                ]
            , time_
                [ textProp "datetime" "2024-01-19T11:53:33Z"
                , textProp "data-local" "true"
                , title_ "14 days ago"
                ][ "2024-01-19 (Fri) 06:53:33" ]
            ]
        , " "
        , a_
            [ class_ "post_no"
            , id_ "post_no_477700"
            , href_ "/leftypol/res/477700.html#477700"
            ][ "No." ]
        , a_
            [ class_ "post_no"
            , href_ "/leftypol/res/477700.html#q477700"
            ][ "477700" ]
    , div_
        [ class_ "body" ]
        (body op_post)
    ]

reply :: Post -> View a
reply post = div_
    [ class_ "postcontainer"
    , id_ "pc477702"
    , textProp "data-board" "leftypol"
    ]
    [ div_
        [ class_ "post reply"
        , id_ "reply_477702"
        ]
        [ p_
            [ class_ "intro" ]
            [ label_
                [ for_ "delete_477702" ]
                [ span_
                    [ class_ "name" ][ "Anonymous" ]
                , time_
                    [ textProp "datetime" "2024-01-19T11:58:26Z"
                    , textProp "data-local" "true"
                    , title_ "17 days ago"
                    ][ "2024-01-19 (Fri) 06:58:26" ]
                ]
            , "&nbsp;"
            , a_
                [ class_ "post_no"
                , id_ "post_no_477702"
                -- , onclick_ "highlightReply(477702)"
                , href_ "/leftypol/res/477700.html#477702"
                ][ "No." ]
            , a_
                [ class_ "post_no"
                -- , onclick_ "citeReply(477702)"
                , href_ "/leftypol/res/477700.html#q477702"
                ][ "477702" ]
            , span_
                [ class_ "mentioned unimportant" ]
                [ a_
                    [ class_ "mentioned-477703"
                    -- , onclick_ "highlightReply('477703');"
                    , href_ "#477703"
                    ][ "&gt;&gt;477703" ]
                ]
            ]
        , div_
            [ class_ "files" ]
            [ div_
                [ class_ "file" ]
                [ p_
                    [ class_ "fileinfo" ]
                    [ span_ []
                        [ "File"
                        , small_ []
                            [ "("
                            , a_
                                [ class_ "hide-image-link"
                                , href_ "javascript:void(0)"
                                ][ "hide" ]
                            , ")"
                            ]
                        , ":"
                        ]
                    , a_
                        [ href_ "/leftypol/src/1705665505794.jpeg"
                        ][ "1705665505794.jpeg" ]
                    , span_
                        [ class_ "details" ]
                        [ "( 403.8 KB, 1280x720,"
                        , a_
                            [ download_ "Aerial-shot-Washington-DC-Palestine.jpeg"
                            , href_ "javascript:void(0)"
                            , title_ "Save as original filename (Aerial-shot-Washington-DC-Palestine.jpeg)"
                            ][ "Aerial-shot-Washington-DC….jpeg" ]
                        , ")"
                        ]
                    ]
                , a_
                    [ href_ "/leftypol/src/1705665505794.jpeg"
                    ]
                    [ img_
                        [ style_ $ Map.fromList
                            [
                                ( "height"
                                , "143px"
                                )
                            ,
                                ( "width"
                                , "255px"
                                )
                            ]
                        , class_ "post-image"
                        , loading_ "lazy"
                        , src_ "/leftypol/thumb/1705665505794.png"
                        , alt_ ""
                        ]
                    ]
                ]
            ]
        , div_
            [ class_ "body" ]
            (body post)
        ]
    ]
