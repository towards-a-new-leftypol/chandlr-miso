{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Data.Maybe (maybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (uriPath)
import System.FilePath ((</>))

import Data.Aeson (FromJSON)
import Data.JSString (pack, append)
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
    , pushURI
    , uriSub
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
import Network.CatalogPostType (CatalogPost)
import qualified Network.CatalogPostType as CatalogPost

import qualified Component.CatalogGrid as Grid


data Model = Model
    { gridModel :: Grid.Model
    , clientModel :: Client.Model
    , current_uri :: URI
    } deriving Eq


initialActionFromRoute :: Model -> URI -> Action
initialActionFromRoute model uri = either (const NoAction) id routing_result
    where
        routing_result = runRoute (Proxy :: Proxy Route) handlers (const uri) model

        handlers = h_latest :<|> h_thread

        h_latest :: Model -> Action
        h_latest = const GetLatest

        h_thread :: Text -> Text -> BoardThreadId -> Model -> Action
        h_thread website board_pathpart board_thread_id _ = GetThread GetThreadArgs {..}


initialModel
    :: JSString
    -> Int
    -> JSString
    -> URI
    -> Model
initialModel pgroot client_fetch_count media_root u = Model
    { gridModel = Grid.initialModel media_root
    , clientModel = Client.Model
        { Client.pgApiRoot = pgroot
        , Client.fetchCount = client_fetch_count
        }
    , current_uri = u
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

    media_root <- getMetadata "media-root" >>=
        return . maybe "undefined" id

    let initial_model = initialModel pg_api_root pg_fetch_count media_root uri

    startApp App
        { model         = initial_model
        , update        = mainUpdate
        , view          = mainView
        , subs          = [ uriSub ChangeURI ]
        , events        = defaultEvents
        , initialAction = initialActionFromRoute initial_model uri
        , mountPoint    = Nothing
        , logLevel      = Off
        }


mainView :: Model -> View Action
mainView model = view
    where
        view =
          either (const page404) id
            $ runRoute (Proxy :: Proxy Route) handlers current_uri model

        handlers = catalog_view :<|> thread_view

        catalog_view :: Model -> View Action
        catalog_view _ = div_ []
            [ h1_ [] [ text "Overboard Catalog" ]
            , Grid.view iGrid (gridModel model)
            ]

        thread_view :: Text -> Text -> BoardThreadId -> Model -> View Action
        thread_view site_name board_pathpart board_thread_id m =
            h1_ [] [ text "Thread View" ]

        page404 :: View Action
        page404 = h1_ [] [ text "404 Not Found" ]


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
            -- mapM_ (consoleLog . toJSString . show) posts
            return $ GridAction $ Grid.DisplayItems posts

mainUpdate (HaveThread Client.Error) m = m <# do
    consoleLog "Getting Thread failed!"
    return NoAction

mainUpdate (HaveThread (Client.HttpResponse {..})) m = m <# do
    consoleLog "Have Thread!"
    return NoAction

mainUpdate GetLatest m = m <# Client.fetchLatest (clientModel m) (iClient HaveLatest)

-- mainUpdate GetThread {..} m = noEff m

mainUpdate (GetThread GetThreadArgs {..}) m = m <# do
    consoleLog $ "Thread " `append` (pack $ show $ board_thread_id)
    pushURI new_current_uri
    Client.getThread (clientModel m) (iClient HaveThread) GetThreadArgs {..}

    where
        new_current_uri :: URI
        new_current_uri = (current_uri m) {
            uriPath = T.unpack website
            </> T.unpack board_pathpart
            </> show board_thread_id
        }

mainUpdate (ChangeURI old_uri) m = m { current_uri = old_uri } <# do
    consoleLog $ "ChangeURI! " `append` (pack $ show $ old_uri)
    return NoAction

mainUpdate (GridAction ga) m =
    Grid.update iGrid ga (gridModel m)
    >>= \gm -> noEff (m { gridModel = gm })

mainUpdate (ClientAction action ca) m =
    Client.update (iClient action) ca (clientModel m)
    >>= \cm -> noEff (m { clientModel = cm })


iGrid :: Grid.Interface Action
iGrid = Grid.Interface
    { Grid.passAction = GridAction
    , Grid.threadSelected = mkGetThread
    }

    where
        mkGetThread :: CatalogPost -> Action
        mkGetThread post = GetThread GetThreadArgs
            { website = CatalogPost.site_name post
            , board_pathpart = CatalogPost.pathpart post
            , board_thread_id = CatalogPost.board_thread_id post
            }

iClient :: (FromJSON a) => (Client.HttpResult a -> Action) -> Client.Interface Action a
iClient action = Client.Interface
    { Client.passAction = ClientAction action
    , Client.returnResult = action
    }

{-
 - TODO:
 -  - Create the thread view
 -      - add routing so when you click in the catalog it goes to the thread
 -          - register onClick ✓
 -          - pevent default and consoleLog the event ✓
 -          - display page
 -          - history api / navigation for browser history ✓
 -      - create component ✓
 -
 -
 -  - make it isomorphic
 -      - move everything before or during this part into common lib
 -}
