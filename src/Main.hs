{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Miso
    ( View (..)
    , consoleLog
    , run
    , miso
    , getURI
    , URI (..)
    )
import Servant.Miso.Router (route)
import Miso.String (MisoString, toMisoString, fromMisoString)
import Servant.API hiding (URI)
import Data.Aeson (decodeStrict)
import Control.Monad.IO.Class (liftIO)
import Language.Javascript.JSaddle.Monad (JSM)
import Network.URI (unEscapeString)

import Common.FrontEnd.Action
import Common.FrontEnd.Routes
import qualified Common.Network.ClientTypes as Client
import Common.FrontEnd.MainComponent
import Common.FrontEnd.Model
import JSFFI.Saddle
    ( getDocument
    , Element (..)
    , Document (..)
    , ParentNode (..)
    , querySelector
    , textContent
    )
import Common.FrontEnd.JSONSettings (fromHtml)

initialActionFromRoute :: Model -> URI -> Action
initialActionFromRoute model uri = either (const NoAction) id routing_result
    where
        routing_result = route (Proxy :: Proxy (Route (View Model Action))) handlers (const uri) model

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
            | search_term m == unescaped_search_query = SearchResults unescaped_search_query
            | otherwise = NotifySearch $ unescaped_search_query

            where
                unescaped_search_query = toMisoString $ unEscapeString $ search_query


getScriptContents :: MisoString -> JSM (Maybe MisoString)
getScriptContents className = do
    doc <- (\(Document d) -> ParentNode d) <$> getDocument

    mElem :: Maybe Element <- querySelector doc $ "." <> (fromMisoString className)

    case mElem of
        Nothing -> return Nothing
        Just e -> (toMisoString <$>) <$> textContent e


getInitialDataPayload :: JSM InitialDataPayload
getInitialDataPayload = do
    rawData <- getScriptContents "initial-data" >>= return . maybe "" id

    maybe
        ( do
            t <- liftIO getCurrentTime
            return $ InitialDataPayload t Nil
        )
        return
        (decode rawData)


    where
        decode :: MisoString -> Maybe InitialDataPayload
        decode = decodeStrict . encodeUtf8 . fromMisoString


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

    initialDataPayload <- getInitialDataPayload

    -- WTF is going on here?
    --
    -- let initial_data =
    --         case some_initial_data of
    --             (ThreadData site _) -> ThreadData site (getPostWithBodies site)
    --             x -> x

    miso $ const $ app jsonSettings uri initialDataPayload
