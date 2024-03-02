{-# LANGUAGE OverloadedStrings #-}

module Component.CatalogGrid
( Model (..)
, initialModel
, Action (..)
, Interface (..)
, view
, update
) where

import Data.Maybe (maybeToList)
import Data.Text (pack, Text)
import qualified Data.Text as T
import Data.JSString (append, JSString)
import Miso
    ( View, div_ , class_ , img_ , href_ , a_
    , src_ , title_ , b_ , span_
    , p_ , id_ , Effect , noEff
    , text, rawHtml, onWithOptions
    , defaultOptions, preventDefault
    , Attribute, emptyDecoder
    )
import Miso.String (toMisoString, MisoString)

import Network.CatalogPostType (CatalogPost)
import qualified Network.CatalogPostType as CatalogPost
import Parsing.EmbedParser (extractVideoId)

data Model = Model
  { display_items :: [ CatalogPost ]
  , media_root :: MisoString
  } deriving Eq

initialModel :: JSString -> Model
initialModel media_root_ = Model
    { display_items = []
    , media_root = toMisoString media_root_
    }

data Action
    = DisplayItems [ CatalogPost ]

data Interface a = Interface
    { passAction :: Action -> a -- We're not using this.
    , threadSelected :: CatalogPost -> a
    }


-- Custom event handler with preventDefault set to True
onClick_ :: a -> Attribute a
onClick_ action = onWithOptions defaultOptions { preventDefault = True } "click" emptyDecoder (const action)

update
    :: Interface a
    -> Action
    -> Model
    -> Effect a Model
update _ (DisplayItems xs) m = noEff (m { display_items = xs })

view :: Interface a -> Model -> View a
view iface model =
    div_
        [ class_ "theme-catalog" ]
        [ div_
            [ class_ "threads" ]
            [ div_
                [ id_ "Grid" ]
                (map (gridItem iface model) (display_items model))
            ]
        ]

gridItem :: Interface a -> Model -> CatalogPost -> View a
gridItem iface m post =
    div_
        [ class_ "thread grid-li grid-size-small" ]
        [ a_
            [ href_ thread_url
            , onClick_ (threadSelected iface post)
            ]
            [ img_
                [ class_ "thread-image"
                , src_ thumb_url
                , title_ ( toMisoString $ show $ CatalogPost.bump_time post )
                ]
            ]
        , div_
            [ class_ "replies" ]
            (
              [ div_
                  [ class_ "meta" ]
                  [ "R: "
                  , b_ [][ text post_count_str ]
                  , "+"
                  ]
              ] ++ (intro subject) ++ body
            )
        ]

  where
    subject :: [ View a ]
    subject = map (text . toMisoString) $ maybeToList $ CatalogPost.subject post

    intro :: [ View a ] -> [ View a ]
    intro [] = []
    intro x = (: []) $ p_
        [ class_ "intro" ]
        [ span_
            [ class_ "subject" ]
            x
        ]

    body :: [ View a ]
    body = map (rawHtml . toMisoString) $ maybeToList $ CatalogPost.body post

    post_count_str :: MisoString
    post_count_str = toMisoString $ (CatalogPost.estimated_post_count post) - 1

    embed_url :: Maybe String
    embed_url =
        (CatalogPost.embed post) >>= Just . (\(Right r) -> r) . extractVideoId . T.unpack

    thumb_url :: MisoString
    thumb_url  =
        case embed_url of
            Nothing ->
                case mthumb_path of
                    -- TODO: what about embeds!?
                    Nothing -> "/static/default_thumbnail.png"
                    Just thumb_path -> (media_root m) `append` (toMisoString thumb_path)
            Just u -> "https://leftychan.net/vi/" <> toMisoString u <> "/0.jpg"

    mthumb_path :: Maybe Text
    mthumb_path = do
        file_name <- CatalogPost.file_name post
        thumb_ext <- CatalogPost.file_thumb_extension post

        return $
            "/" <> CatalogPost.site_name post
            <> "/" <> CatalogPost.pathpart post
            <> "/" <> (pack $ show $ CatalogPost.board_thread_id post)
            <> "/thumbnail_" <> file_name
            <> "." <> thumb_ext

    thread_url :: MisoString
    thread_url = toMisoString $ T.intercalate "/"
      [ CatalogPost.site_name post
      , CatalogPost.pathpart post
      , pack $ show $ CatalogPost.board_thread_id post
      ]
