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
  ( View
  , class_
  , action_
  , method_
  , input_
  , type_
  , value_
  , name_
  , form_
  , onChange
  , onSubmit
  , Effect
  , (<#)
  , consoleLog
  , noEff
  )
import Data.JSString (pack)
import qualified Network.Client as Client
import Network.Http (HttpResult (..))
import Control.Concurrent.MVar (tryTakeMVar, takeMVar, putMVar, swapMVar)

import Common.Component.Search.SearchTypes

update :: Interface a -> Action -> Model -> Effect a Model
update iface (SearchChange q) model = model { searchTerm = q } <# do
    consoleLog $ "SearchChange " <> q
    m_search_query <- tryTakeMVar (searchVar model)

    case m_search_query of
        Nothing -> putMVar (searchVar model) q
        Just _ -> swapMVar (searchVar model) q >> return ()

    return $ (passAction iface) NoAction

update iface OnSubmit model = model <# do
    search_query <- takeMVar (searchVar model)
    consoleLog $ "Submit! " <> search_query
    Client.search (clientModel model) search_query (clientIface iface)

update iface (ChangeAndSubmit search_query) model = model { searchTerm = search_query } <# do
    _ <- swapMVar (searchVar model) search_query
    return $ (passAction iface) OnSubmit

update iface (SearchResult result) model = model <# do
    consoleLog $ "Received search results!"

    case result of
        Error -> do
            consoleLog $ "Error!"
            return $ passAction iface $ PassPostsToSelf []

        HttpResponse {..} -> do
            consoleLog $ (pack $ show $ status_code) <> " " <> (pack $ status_text)
            consoleLog $ (pack $ show $ body)

            case body of
                Just catlg_posts -> return $ passAction iface $ PassPostsToSelf catlg_posts
                Nothing -> return $ passAction iface $ PassPostsToSelf []

update iface (PassPostsToSelf search_results) model = model { displayResults = search_results } <#
    (return $ (searchResults iface) (searchTerm model))

update _ NoAction m = noEff m


view :: Interface a -> Model -> View a
view iface m = form_
    [ class_ "search_form"
    , action_ "/search"
    , method_ "GET"
    , onSubmit $ pass $ OnSubmit
    ]
    [ input_
        [ type_ "submit"
        , value_ "🔍"
        ]
    , input_
        [ type_ "text"
        , name_ "search"
        , onChange $ pass . SearchChange
        ]
    ]

    where
        pass = passAction iface
