{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso
    ( View
    , startApp
    , App (..)
    , h1_
    , div_
    , text
    , Effect
    --, (#>)
    , noEff
    , defaultEvents
    , LogLevel (Off)
    )

import qualified Component.CatalogGrid as Grid

data Model = Model
    { gridModel :: Grid.Model
    } deriving Eq

data Action
    = GridAction Grid.Action
    | NoAction

initialModel :: Model
initialModel = Model
    { gridModel = Grid.initialModel
    }

main :: IO ()
main = startApp App
    { model         = initialModel
    , update        = mainUpdate
    , view          = mainView
    , subs          = []
    , events        = defaultEvents
    , initialAction = NoAction
    , mountPoint    = Nothing
    , logLevel      = Off
    }

mainView :: Model -> View Action
mainView model =
    div_ []
        [ h1_ [] [ text "Hello World" ]
        , Grid.view iGrid (gridModel model)
        ]

mainUpdate :: Action -> Model -> Effect Action Model
mainUpdate = const noEff

iGrid :: Grid.Interface Action
iGrid = Grid.Interface
    { Grid.passAction = GridAction
    , Grid.selectThread = ()
    }

{-
 - TODO:
 -  - Create Hello World page render ✓
 -  - Create CatalogGrid component (static at first) ✓
 -  - Get postgrest url from page header and perform an initial xhr request
 -}
