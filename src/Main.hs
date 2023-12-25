{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

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
    , URI
    , runRoute
    , getCurrentURI
    )

import Data.Proxy
import Servant.API

import Action
import Routes

import qualified Component.CatalogGrid as Grid

data Model = Model
    { gridModel :: Grid.Model
    } deriving Eq

initialActionFromRoute :: Model -> URI -> Action
initialActionFromRoute model uri = either (const NoAction) id routing_result
    where
        routing_result = runRoute (Proxy :: Proxy Route) handlers (const uri) model

        handlers = h_latest :<|> h_thread

        h_latest :: Model -> Action
        h_latest = const GetLatest

        h_thread :: String -> String -> BoardThreadId -> Model -> Action
        h_thread board website board_thread_id _ = GetThread {..}

initialModel :: Model
initialModel = Model
    { gridModel = Grid.initialModel
    }

main :: IO ()
main = do
    uri <- getCurrentURI

    startApp App
        { model         = initialModel
        , update        = mainUpdate
        , view          = mainView
        , subs          = []
        , events        = defaultEvents
        , initialAction = initialActionFromRoute initialModel uri
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
 -
 -  - do I need to move out everything into another project called chandlr-common?
 -      - if I want to use the isomorphic feature of miso, then yes
 -
 -  - add a router first
 -      - go to the next page and do xhr for the content
 -          - before we do xhr we need the postgrest url,
 -              - how do we tackle a config?
 -  - make it isomorphic
 -      - move everything before or during this part into common lib
 -}
