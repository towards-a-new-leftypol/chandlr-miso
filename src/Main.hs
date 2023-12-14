{-# LANGUAGE OverloadedStrings #-}

module Main where

import Miso
    ( View
    , startApp
    , App (..)
    , h1_
    , text
    , Effect
    --, (#>)
    , noEff
    , defaultEvents
    )

data Unit = Unit
    deriving Eq

type Model = Unit

type Action = Unit

main :: IO ()
main = startApp App
    { model         = Unit
    , update        = mainUpdate
    , view          = mainView
    , subs          = []
    , events        = defaultEvents
    , initialAction = Unit
    , mountPoint    = Nothing
    }

mainView :: Model -> View Action
mainView = const $ h1_ [] [ text "Hello World" ]

mainUpdate :: Action -> Model -> Effect Action Model
-- mainUpdate = const $ (#>) (pure Unit) -- this is an infinite loop!
mainUpdate = const noEff

{-
 - TODO:
 -  - Create Hello World page render ✓
 -  - Create CatalogGrid component (static at first)
 -}

{-
 -
 - TODO:
 - - Initial page just needs to display the search bar, fetch catalog and
 -   display it
 -  Make Model
 -      - single value representing that we need to go fetch the initial page
 -  
 -  maybe immediately create the CatalogGrid component with its own model? That
 -  would allow us to swap it out with a thread / overboard component later, and
 -  tell it what to render
 -}
