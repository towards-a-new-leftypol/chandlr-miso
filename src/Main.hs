{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Data.Maybe (maybe)

import Miso
    ( View
    , startApp
    , App (..)
    , h1_
    , div_
    , text
    , Effect
    --, (#>)
    , (<#)
    , noEff
    , defaultEvents
    , LogLevel (Off)
    , URI
    , runRoute
    , getCurrentURI
    , consoleLog
    --, MisoString (..)
    )
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Types (toJSString, fromJSString, Element, JSString)
import GHCJS.DOM.ParentNode (querySelector)
import GHCJS.DOM.Element (getAttribute)
import Servant.API

import Action
import Routes
import qualified Network.Client as Client

import qualified Component.CatalogGrid as Grid


data Model = Model
    { gridModel :: Grid.Model
    , clientModel :: Client.Model
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


initialModel
    :: JSString
    -> Int
    -> Model
initialModel pgroot client_fetch_count = Model
    { gridModel = Grid.initialModel
    , clientModel = Client.Model
        { Client.pgApiRoot = pgroot
        , Client.fetchCount = client_fetch_count
        }
    }

getMetadata :: String -> IO (Maybe JSString)
getMetadata key = do
    doc <- currentDocument

    mElem :: Maybe Element <- case doc of
        Nothing -> return Nothing
        Just d -> querySelector d $ "meta[name='" ++ key ++ "']"

    case mElem of
        Nothing -> return Nothing
        Just el -> getAttribute el ("content" :: JSString)

main :: IO ()
main = do
    consoleLog "Hello World!"

    uri <- getCurrentURI

    consoleLog $ toJSString $ show uri

    pg_api_root <- getMetadata "postgrest-root" >>=
        return . maybe "http://localhost:2000" id
    consoleLog pg_api_root

    pg_fetch_count <- getMetadata "postgrest-fetch-count" >>=
        return . maybe 1000 (read . fromJSString)

    let initial_model = initialModel pg_api_root pg_fetch_count

    startApp App
        { model         = initial_model
        , update        = mainUpdate
        , view          = mainView
        , subs          = []
        , events        = defaultEvents
        , initialAction = initialActionFromRoute initial_model uri
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
mainUpdate NoAction m = noEff m
mainUpdate (HaveLatest Client.Error) m = m <# do
    consoleLog "Getting Latest failed!"
    return NoAction

mainUpdate (HaveLatest (Client.HttpResponse {..})) m = m <#
    case body of
        Nothing -> do
            consoleLog "Didn't get anything back from API"
            return NoAction
        Just posts -> do
            mapM_ (consoleLog . toJSString . show) posts
            return $ GridAction $ Grid.DisplayItems posts

mainUpdate GetLatest m = m <# Client.fetchLatest (clientModel m) iClient

mainUpdate GetThread {..} m = noEff m

mainUpdate (GridAction ga) m =
    Grid.update iGrid ga (gridModel m)
    >>= \gm -> noEff (m { gridModel = gm })

mainUpdate (ClientAction ca) m =
    Client.update iClient ca (clientModel m)
    >>= \cm -> noEff (m { clientModel = cm })


iGrid :: Grid.Interface Action
iGrid = Grid.Interface
    { Grid.passAction = GridAction
    , Grid.selectThread = ()
    }

iClient :: Client.Interface Action
iClient = Client.Interface
    { Client.passAction = ClientAction
    , Client.returnResult = HaveLatest
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
