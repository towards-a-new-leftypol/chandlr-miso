{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

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
  , consoleError
  , Component
  , defaultEvents
  , modify
  , issue
  , io_
  , get
  , publish
  , subscribe
  )
import qualified Miso as M
import Miso.String (toMisoString)
import Data.Aeson (Result(Success, Error))

import Common.Component.Search.SearchTypes
import Common.Component.Search.View
import qualified Common.Network.ClientTypes as Client
import qualified Network.Client as Client

pattern Sender :: Client.Sender
pattern Sender = "search"

update :: Action -> Effect Model Action
update Initialize = do
    subscribe Client.clientOutTopic SearchResult
    subscribe searchTopic OnMessage

update (SearchChange q) =
    modify (\m -> m { searchTerm = q })

update OnSubmit = do
    model <- get

    let search_query = searchTerm model

    io_ $ consoleLog $ "Submit! " <> search_query

    publish Client.clientInTopic $ (Sender, Client.Search search_query)
    -- notify Client.app (Client.Search search_query)

update (ChangeAndSubmit search_query) = do
    issue $ SearchChange search_query
    issue $ OnSubmit

update (SearchResult (Success (Client.ReturnResult Sender result))) =
    Client.helper result $ \search_results ->
        modify (\m -> m { displayResults = search_results })

update (SearchResult (Success (Client.ReturnResult _ _))) = return ()

update (SearchResult (Error msg)) =
    io_ $ consoleError (toMisoString msg)

update (OnMessage (Success query)) = issue $ ChangeAndSubmit query

update (OnMessage (Error msg)) =
    io_ $ consoleError $ "Search OnMessage decode error: " <> (toMisoString msg)

app :: Component Model Action
app = M.Component
    { M.model = Model "" []
    , M.update = update
    , M.view = view
    , M.subs = []
    , M.events = defaultEvents
    , M.styles = []
    , M.initialAction = Just Initialize
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    }
