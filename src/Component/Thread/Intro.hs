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
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Network.PostType (Post)
import qualified Network.PostType as Post


formatUTC :: UTCTime -> JSString
formatUTC time = toMisoString $
    formatTime defaultTimeLocale "%Y-%m-%d (%a) %T" time

intro :: Post -> UTCTime -> View a
intro post current_time = span_
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
        , title_ $ toMisoString $ timeAgo current_time creation_time
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


-- Convert UTCTime to a human-readable string
timeAgo :: UTCTime -> UTCTime -> String
timeAgo currentTime pastTime =
    let diff = realToFrac $ diffUTCTime currentTime pastTime
    in humanReadableTimeDiff diff

-- Helper function to correctly format singular and plural units
formatTimeUnit :: (Integral a, Show a) => a -> String -> String
formatTimeUnit value unit
    | value == 1 = show value ++ " " ++ unit ++ " ago"
    | otherwise  = show value ++ " " ++ unit ++ "s ago"

-- Convert time difference in seconds to a human-readable format
humanReadableTimeDiff :: Double -> String
humanReadableTimeDiff diff
    | diff < 60 = "Just now"
    | diff < 3600 = formatTimeUnit (truncate (diff / 60) :: Int) "minute"
    | diff < 86400 = formatTimeUnit (truncate (diff / 3600) :: Int) "hour"
    | diff < 604800 = formatTimeUnit (truncate (diff / 86400) :: Int) "day"
    | diff < 2592000 = formatTimeUnit (truncate (diff / 604800) :: Int) "week"
    | diff < 31536000 = formatTimeUnit (truncate (diff / 2592000) :: Int) "month"
    | otherwise = formatTimeUnit (truncate (diff / 31536000) :: Int) "year"
