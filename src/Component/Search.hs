{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Component.Search
( view
, Interface (..)
, update
, Model (..)
, Action (..)
) where

import Miso
  ( Effect
  , (<#)
  , consoleLog
  , noEff
  )
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (tryTakeMVar, takeMVar, putMVar, swapMVar)
import Miso.String (toMisoString)

import Common.Network.HttpTypes (HttpResult (..))
import Common.Component.Search.SearchTypes
import Common.Component.Search.View
import qualified Network.Client as Client

update :: Interface a -> Action -> Model -> Effect Model a ()
update iface (SearchChange q) model = model { searchTerm = q } <# do
    consoleLog $ "SearchChange " <> q

    liftIO $ do
        m_search_query <- tryTakeMVar (searchVar model)

        case m_search_query of
            Nothing -> putMVar (searchVar model) q
            Just _ -> swapMVar (searchVar model) q >> return ()

        return $ (passAction iface) NoAction

update iface OnSubmit model = model <# do
    search_query <- liftIO $ takeMVar (searchVar model)
    consoleLog $ "Submit! " <> search_query
    Client.search (clientModel model) search_query (clientIface iface)

update iface (ChangeAndSubmit search_query) model = model { searchTerm = search_query } <# do
    _ <- liftIO $ liftIO $ swapMVar (searchVar model) search_query
    return $ (passAction iface) OnSubmit

update iface (SearchResult result) model = model <# do
    consoleLog $ "Received search results!"

    case result of
        Error -> do
            consoleLog $ "Error!"
            return $ passAction iface $ PassPostsToSelf []

        HttpResponse {..} -> do
            consoleLog $ (toMisoString $ show $ status_code) <> " " <> (toMisoString $ status_text)
            consoleLog $ (toMisoString $ show $ body)

            case body of
                Just catlg_posts -> return $ passAction iface $ PassPostsToSelf catlg_posts
                Nothing -> return $ passAction iface $ PassPostsToSelf []

update iface (PassPostsToSelf search_results) model = model { displayResults = search_results } <#
    (return $ (searchResults iface) (searchTerm model))

update _ NoAction m = noEff m
