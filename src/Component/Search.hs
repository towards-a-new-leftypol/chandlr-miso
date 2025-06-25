{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Component.Search
( view
, update
, Model (..)
, Action (..)
, app
) where

import Miso
  ( Effect
  , consoleLog
  , Component
  , defaultEvents
  , modify
  , issue
  , io_
  , get
  , notify
  )
import qualified Miso as M
import Miso.String (toMisoString)

import Common.Network.HttpTypes (HttpResult (..))
import Common.Component.Search.SearchTypes
import Common.Component.Search.View
import qualified Network.Client as Client
import Common.Network.CatalogPostType (CatalogPost)


update :: Action -> Effect Model Action
update (SearchChange q) =
    modify (\m -> m { searchTerm = q })

update OnSubmit = do
    model <- get

    let search_query = searchTerm model

    io_ $ do
        consoleLog $ "Submit! " <> search_query
        notify Client.app (clientInterface, Client.Search search_query)

update (ChangeAndSubmit search_query) = do
    issue $ SearchChange search_query
    issue $ OnSubmit

update (SearchResult result) = do
    io_ $ consoleLog $ "Received search results!"

    case result of
        Error -> io_ $ consoleLog "Error!"

        HttpResponse {..} -> do
            io_ $ do
                consoleLog $ (toMisoString $ show $ status_code) <> " " <> (toMisoString $ status_text)
                consoleLog $ (toMisoString $ show $ body)

            case body of
                Just search_results ->
                    modify (\m -> m { displayResults = search_results })
                Nothing -> return ()


clientInterface :: Client.Interface "search" Model Action [ CatalogPost ]
clientInterface = Client.Interface SearchResult app


app :: Component "search" Model Action
app = M.Component
    { M.model = Model "" []
    , M.update = update
    , M.view = view
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Nothing
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    }
