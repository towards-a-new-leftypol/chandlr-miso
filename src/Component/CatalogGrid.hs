{-# LANGUAGE OverloadedStrings #-}

module Component.CatalogGrid
( Model
, initialModel
, Action (..)
, Interface (..)
, view
, update
) where

import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.JSString (append)
import Miso
    ( View , div_ , class_ , img_ , href_ , a_
    , src_ , alt_ , title_ , strong_ , span_
    , p_ , br_ , id_ , Effect , noEff
    , text, rawHtml
    )

import Network.CatalogPostType (CatalogPost)
import qualified Network.CatalogPostType as CatalogPost
import Miso.String (toMisoString, MisoString)

data Model = Model
  { displayItems :: [ CatalogPost ]
  } deriving Eq

initialModel :: Model
initialModel = Model { displayItems = [] }

data Action = DisplayItems [ CatalogPost ]

data Interface a = Interface
    { passAction   :: Action -> a
    , selectThread :: ()
    }

update
    :: Interface a
    -> Action
    -> Model
    -> Effect a Model
update _ (DisplayItems xs) m = noEff (m { displayItems = xs })
update _ _ m = noEff m

view :: Interface a -> Model -> View a
view iface model =
    div_
        [ class_ "theme-catalog" ]
        [ div_
            [ class_ "threads" ]
            [ div_
                [ id_ "Grid" ]
                (map gridItem (displayItems model))
            ]
        ]

gridItem :: CatalogPost -> View a
gridItem post =
    div_
        [ class_ "mix" ]
        [ div_
            [ class_ "thread grid-li grid-size-small" ]
            [ a_
                [ href_ thread_url ]
                [ img_
                    [ class_ "thread-image"
                    , src_ "/a/thumb/1111111111111.png"
                    , alt_ "Opening post image"
                    , title_ ( toMisoString $ show $ CatalogPost.bump_time post )
                    ]
                ]
            , div_
                [ class_ "replies" ]
                (
                  [ strong_ [][ text post_count_str ]
                  , p_
                      [ class_ "intro" ]
                      [ span_
                          [ class_ "subject" ]subject
                      ]
                  ] ++ body
                )
            ]
        ]

  where
    subject :: [ View a ]
    subject = map (text . toMisoString) $ maybeToList $ CatalogPost.subject post

    body :: [ View a ]
    body = map (rawHtml . toMisoString) $ maybeToList $ CatalogPost.body post

    post_count_str :: MisoString
    post_count_str = "R: " `append` (toMisoString $ CatalogPost.post_count post)

    thread_url :: MisoString
    thread_url = toMisoString $ intercalate "/"
      [ CatalogPost.site_name post
      , CatalogPost.pathpart post
      , show $ CatalogPost.board_thread_id post
      ]
