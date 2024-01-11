{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.MVar (takeMVar)
import Data.Proxy
import Data.Text (Text)
import Control.Monad (void)
import Control.Concurrent (forkIO)

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
    , effectSub
    , consoleLog
    --, MisoString (..)
    )
import GHCJS.DOM.Types (toJSString)
import Servant.API

import Action
import Routes
import Network.Client as Client

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
    consoleLog "Hello World!"

    uri <- getCurrentURI

    consoleLog $ toJSString $ show uri

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
mainUpdate NoAction m = noEff m
mainUpdate (HaveLatest Client.Error) m = noEff m

mainUpdate (HaveLatest (Client.HttpResponse {..})) m = m <# do
    case body of
        Nothing -> do
            putStrLn "Didn't get anything back from API"
            consoleLog "Didn't get anything back from API"
        Just txt -> do
            print txt
            consoleLog $ toJSString txt

    return NoAction

mainUpdate (NewConnection (_, resultVar)) m = effectSub m $ \sink -> do
    void $ forkIO $ do
        result :: Client.HttpResult Text <- takeMVar resultVar
        sink $ HaveLatest result

mainUpdate GetLatest m = m <# do
    putStrLn "Getting Latest!"

    stuff <- Client.http
        "http://localhost:3000/posts?limit=10"
        Client.GET
        Nothing

    return $ NewConnection stuff

{-
mainUpdate GetLatest m =
    Client.http
        "/posts?limit=10"
        Client.GET
        Nothing

    <# \(abort, resultVar) ->
        effectSub m (httpSub resultVar) <> noEff m

  where
    httpSub :: MVar (Client.HttpResult Text) -> Sink Action -> IO ()
    httpSub resultVar sink = do
        result :: Client.HttpResult Text <- liftIO $ takeMVar resultVar
        liftIO $ sink (HaveLatest result)
-}

{-
mainUpdate GetLatest m = do
    (abort, resultVar) <- Client.http
            "/posts?limit=10"
            Client.GET
            Nothing

    -- return noEff m
    return $ effectSub m (httpSub resultVar) <> noEff m -- { abortAction = Just abort }

  where
    httpSub :: MVar (Client.HttpResult Text) -> Sink Action -> IO ()
    httpSub resultVar sink = do
        result :: Client.HttpResult Text <- liftIO $ takeMVar resultVar
        liftIO $ sink (HaveLatest result)
-}

mainUpdate GetThread {..} m = noEff m
mainUpdate (GridAction ga) m =
    Grid.update iGrid ga (gridModel m)
    >>= \gm -> noEff (m { gridModel = gm })


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
