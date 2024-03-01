{-# LANGUAGE OverloadedStrings #-}

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
import GHCJS.DOM.Types (JSString)

data Action = SearchChange JSString | OnSubmit | NoAction

data Model = Model
  { search_term :: JSString
  } deriving Eq

data Interface a = Interface
  { passAction :: Action -> a
  }


update :: Interface a -> Action -> Model -> Effect a Model
update iface (SearchChange q) model = model { search_term = q } <# do
  consoleLog q
  return $ (passAction iface) NoAction

update iface OnSubmit model = model <# do
  consoleLog $ "Submit!" <> search_term model
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
