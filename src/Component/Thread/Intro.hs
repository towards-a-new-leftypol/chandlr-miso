{-# LANGUAGE OverloadedStrings #-}

module Component.Thread.Intro where

import Miso
  ( View
  , text
  , href_
  , a_
  , class_
  , id_
  , textProp
  , title_
  , span_
  , time_
  )

import GHCJS.DOM.Types (JSString)
import Data.Foldable (toList)
import Miso.String (toMisoString)

import Network.PostType (Post)
import qualified Network.PostType as Post

intro :: Post -> View a
intro post = span_
  [ class_ "intro" ]
  ( subject ++
    [ span_
        [ class_ "name" ][ "Anonymous" ]
    -- TODO: Add flags (don't have that data in the db yet)
    , time_
        [ textProp "datetime" "2024-01-19T11:53:33Z"
        , textProp "data-local" "true"
        , title_ "14 days ago"
        ][ "2024-01-19 (Fri) 06:53:33" ]
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
    ]
  )

  where
    subject :: [ View a ]
    subject = map (mkSubject . toMisoString) $ toList $ Post.subject post

    mkSubject :: JSString -> View a
    mkSubject s = span_ 
      [ class_ "subject" ]
      [ text s ]
