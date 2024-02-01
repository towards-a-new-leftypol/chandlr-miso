module Component.ThreadView
( Model (..)
, initialModel
, Action (..)
, update
, view
) where

import Miso
  ( View
  , Effect
  , div_
  , text
  , h1_
  , noEff
  )
import Miso.String (toMisoString)
import GHCJS.DOM.Types (JSString)

import Network.SiteType (Site)
import qualified Network.SiteType as Site

data Model = Model
  { site :: Site
  , media_root :: JSString
  } deriving Eq

initialModel :: JSString -> Site -> Model
initialModel mroot s = Model
    { site = s
    , media_root = mroot
    }

data Action = RenderSite Site

update :: Action -> Model -> Effect a Model
update (RenderSite s) m = noEff (m { site = s })

view :: Model -> View a
view m =
  div_
    []
    [ h1_ [] [ text $ toMisoString $ Site.name $ site m ]
    ]
