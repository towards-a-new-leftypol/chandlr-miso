{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Data.Proxy
import Data.Text (Text)
import Miso
    ( View (..)
    , consoleLog
    , run
    , miso
    , getURI
    , URI (..)
    )
import Servant.Miso.Router (route)
import Miso.String (toMisoString, fromMisoString)
import Servant.API hiding (URI)
import Language.Javascript.JSaddle.Monad (JSM)
import Network.URI (unEscapeString)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)

import Common.FrontEnd.Action
import Common.FrontEnd.Routes
import qualified Common.Network.ClientTypes as Client
import Common.FrontEnd.MainComponent
import Common.FrontEnd.Model
import Common.FrontEnd.JSONSettings (fromHtml)
import Common.FrontEnd.Types (InitialDataPayload (InitialDataPayload))
import Utils (getMetadata)

initialActionFromRoute :: Model -> URI -> Action
initialActionFromRoute model uri = fromRight NoAction routing_result
    where
        routing_result =
            route
                (Proxy :: Proxy (Route (View Model Action)))
                handlers
                (const uri)
                model

        handlers = h_latest :<|> h_thread :<|> h_search

        h_latest :: Model -> Action
        h_latest = const $ GoToTime $ current_time model

        h_thread :: Text -> Text -> BoardThreadId -> Model -> Action
        h_thread website board_pathpart board_thread_id _ =
            GetThread Client.GetThreadArgs
                { Client.website = toMisoString website
                , Client.board_pathpart = toMisoString board_pathpart
                , Client.board_thread_id = board_thread_id
                }

        h_search :: Maybe String -> Model -> Action
        h_search Nothing m = GoToTime $ current_time m
        h_search (Just search_query) m
            | search_term m == unescaped_search_query =
                SearchResults (unescaped_search_query, [])
            | otherwise = NotifySearch $ unescaped_search_query

            where
                unescaped_search_query =
                    toMisoString $ unEscapeString $ search_query


#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif


main :: IO ()
main = run mainMain

mainMain :: JSM ()
mainMain = do
    consoleLog "Haskell begin."

    jsonSettings <- fromHtml

    uri <- getURI

    currentTime <- getMetadata "timestamp"
        >>= maybe (liftIO getCurrentTime) (iso8601ParseM . fromMisoString)

    miso $ const $ app jsonSettings uri $
        InitialDataPayload currentTime undefined
