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
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Network.PostType (Post)
import qualified Network.PostType as Post

formatUTC :: UTCTime -> JSString
formatUTC time = toMisoString $
    formatTime defaultTimeLocale "%Y-%m-%d (%a) %T" time

intro :: Post -> View a
intro post = span_
  [ class_ "intro" ]
  ( subject ++
    [ " "
    , span_
        [ class_ "name" ][ text name ]
    -- TODO: Add flags (don't have that data in the db yet)
    , " "
    , time_
        [ textProp "datetime" $ toMisoString $ show $ creation_time
        , textProp "data-local" "true"
        -- , title_ "14 days ago"
        ][ text $ formatUTC creation_time ]
    , " "
    , a_
        [ class_ "post_no"
        , id_ "post_no_477700"
        , href_ "/leftypol/res/477700.html#477700"
        ][ "No." ]
    , a_
        [ class_ "post_no"
        , href_ "/leftypol/res/477700.html#q477700"
        ][ text $ toMisoString $ show $ Post.board_post_id post ]
    ]
  )

  where
    creation_time = Post.creation_time post

    subject :: [ View a ]
    subject = map (mkSubject . toMisoString) $ toList $ Post.subject post

    name :: JSString
    name = maybe "Anonymous" toMisoString $ Post.name post

    mkSubject :: JSString -> View a
    mkSubject s = span_ 
      [ class_ "subject" ]
      [ text s ]
