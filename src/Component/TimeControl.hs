{-# LANGUAGE OverloadedStrings #-}

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

import Miso.String (toMisoString)
import Data.Time.Clock (UTCTime)
import GHCJS.DOM.Types (JSString)

data Time
  = Now
  | At UTCTime
  | NoAction
  | SlideInput JSString
  | SlideChange JSString
  deriving Show

data Interface a = Interface
    { passAction :: Time -> a
    }

view :: Interface a -> View a
view iface =
    div_
        [ class_ "time-control"
        ]
        [ input_
            [ class_ "time-slider"
            , type_ "range"
            , min_ "-500"
            , max_ "0"
            , step_ "1"
            , value_ "0"
            , onInput $ pass SlideInput
            , onChange $ pass SlideChange
            ]
        ]

    where
      pass action = \t -> passAction iface $ action t

update
    :: Interface a
    -> Time
    -> ()
    -> Effect a ()
update iface (At time) m = m <# do
  consoleLog $ toMisoString $ show time

  return $ (passAction iface) NoAction

update iface (SlideInput time) m = m <# do
  consoleLog $ "Input: " <> time

  return $ (passAction iface) NoAction

update iface (SlideChange time) m = m <# do
  consoleLog $ "Change: " <> time

  return $ (passAction iface) NoAction

update _ _ m = noEff m
