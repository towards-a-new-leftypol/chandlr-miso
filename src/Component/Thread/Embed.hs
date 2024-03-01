{-# LANGUAGE OverloadedStrings #-}

module Component.Thread.Embed where

import Miso
  ( View
  , div_
  , class_
  , a_
  , href_
  , target_
  , img_
  , style_
  , src_
  , span_
  )

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Data.JSString (JSString, pack)

import qualified Network.PostType as Post
import Network.PostType (Post)
import Parsing.EmbedParser (extractVideoId)

embed :: Post -> View a
embed post = div_
    [ class_ "video-container" ]
    [ a_
        [ href_ $ "https://youtu.be/" <> video_id
        , target_ "_blank"
        , class_ "file"
        ]
        [ img_
            [ style_ $ Map.fromList
                [
                    ( "height"
                    , "190px"
                    )
                ,
                    ( "width"
                    , "255px"
                    )
                ]
            , src_ ("https://leftychan.net/vi/" <> video_id <> "/0.jpg")
            , class_ "post-image"
            ]
        ]
    , span_ [][ "[Embed]" ]
    ]

    where
      video_id :: JSString
      video_id = pack $ fromJust $
          (Post.embed post) >>= Just . (\(Right r) -> r) . extractVideoId . unpack
