{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Component.TimeControl where

import Miso
    ( View
    , div_
    , class_
    , input_
    , step_
    , min_
    , max_
    , type_
    , value_
    , (<#)
    , consoleLog
    , Effect
    , noEff
    , onInput
    , onChange
    )

import Miso.String (toMisoString, fromMisoString)
import GHCJS.DOM.Types (JSString)
import Data.Time.Clock
  ( UTCTime (..)
  , getCurrentTime
  , diffUTCTime
  , addUTCTime
  )
import Data.Time.Calendar (fromGregorian, Day)

data Time
  = Now
  | NoAction
  | SlideInput JSString
  | SlideChange JSString
  deriving Show

data Interface a = Interface
    { passAction :: Time -> a
    , goTo :: UTCTime -> a
    }

data Model = Model
  { whereAt :: Integer
  } deriving Eq

initialModel :: Integer -> Model
initialModel = Model

view :: Interface a -> Model -> View a
view iface m =
    div_
        [ class_ "time-control"
        ]
        [ input_
            [ class_ "time-slider"
            , type_ "range"
            , min_ "-500"
            , max_ "0"
            , step_ "1"
            , value_ $ toMisoString $ show (whereAt m)
            , onInput $ pass SlideInput
            , onChange $ pass SlideChange
            ]
        ]

    where
      pass action = \t -> passAction iface $ action t

update
    :: Interface a
    -> Time
    -> Model
    -> Effect a Model
update iface (SlideInput time) m = m <# do
  consoleLog $ "Input: " <> time

  return $ (passAction iface) NoAction

update iface (SlideChange nstr) m = m { whereAt = n } <# do
  consoleLog $ "Change: " <> nstr

  now <- getCurrentTime

  return $ (goTo iface) $ interpolateTimeHours n now

  where
    n :: Integer
    n = read $ fromMisoString nstr


update _ _ m = noEff m


earliestDate :: Day
earliestDate = fromGregorian 2020 12 1


-- Linear interpolation function using hours
interpolateTimeHours :: Integer -> UTCTime -> UTCTime
interpolateTimeHours n currentTime
  | n == 0 = currentTime
  | otherwise = addUTCTime (fromIntegral hoursToAdjust * secondsInHour) currentTime

  where
    targetDate = UTCTime earliestDate 0

    -- Calculate the total number of hours between the current time and the target date
    totalHours = diffUTCTime currentTime targetDate / secondsInHour

    -- Calculate the number of hours to adjust based on linear interpolation
    hoursToAdjust :: Integer
    hoursToAdjust = round $ totalHours * (fromIntegral n / 500.0)

    -- One hour in seconds
    secondsInHour = 3600
