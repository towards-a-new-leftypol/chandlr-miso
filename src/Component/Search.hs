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
import Component.Search.SearchTypes

update :: Interface a -> Action -> Model -> Effect a Model
update iface (SearchChange q) model = model { searchTerm = q } <# do
  consoleLog q
  return $ (passAction iface) NoAction

update iface OnSubmit model = model <# do
  consoleLog $ "Submit! " <> searchTerm model
  Client.search (clientModel model) (searchTerm model) (clientIface iface)

update iface (SearchResult result) model = model <# do
  consoleLog $ "Search result"
  case result of
    Error -> consoleLog $ "Error!"
    HttpResponse {..} -> do
      consoleLog $ (pack $ show $ status_code) <> " " <> (pack $ status_text)
      consoleLog $ (pack $ show $ body)

  return $ (passAction iface) NoAction

update _ NoAction m = noEff m

view :: Interface a -> View a
view iface = form_
    [ class_ "search_form"
    , action_ "/search"
    , method_ "GET"
    , onSubmit $ pass OnSubmit
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
