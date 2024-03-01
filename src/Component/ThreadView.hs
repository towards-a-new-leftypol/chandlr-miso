{-# LANGUAGE OverloadedStrings #-}

module Component.ThreadView
( Model (..)
, initialModel
, Action (..)
, update
, view
, Interface (..)
) where

import Miso
  ( View
  , Effect
  , div_
  , text
  , h1_
  , noEff
  , class_
  , id_
  , h2_
  , Attribute
  , (<#)
  , consoleLog
  )

import Data.Text (Text)
import Miso.String (toMisoString)
import GHCJS.DOM.Types (JSString)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime, getCurrentTime)
import Data.Time.Calendar (Day (..))

import Network.SiteType (Site)
import qualified Network.SiteType as Site
import Network.PostType (Post)
import qualified Network.PostType as Post
import qualified Network.BoardType as Board
import Network.BoardType (Board)
import qualified Network.ThreadType as Thread
import Network.ThreadType (Thread)
import Component.Thread.Files (files)
import Component.Thread.Intro (intro)
import Component.Thread.Embed (embed)
import Component.Thread.Model
import Parsing.BodyParser
import qualified Component.BodyRender as Body

initialModel :: JSString -> Site -> Model
initialModel mroot s = Model
    { site = s
    , post_bodies = []
    , media_root = mroot
    , current_time = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
    }

data Action
    = RenderSite Site
    | UpdatePostBodies UTCTime [ PostWithBody ]

data Interface a = Interface { passAction :: Action -> a }

update :: Interface a -> Action -> Model -> Effect a Model
update iface (RenderSite s) m = m { site = s } <# do
    bodies <- mapM getBody (map Post.body posts)

    mapM_ (consoleLog . toMisoString . show) bodies

    now <- getCurrentTime

    return $ passAction iface $ UpdatePostBodies now $ zip posts bodies

    where
        getBody :: Maybe Text -> IO [ PostPart ]
        getBody Nothing = return []
        getBody (Just b) = parsePostBody b

        posts :: [ Post ]
        posts = Thread.posts $ head $ Board.threads $ head $ Site.boards s
--update (RenderSite s) m = noEff (m { site = s })

update _ (UpdatePostBodies t pwbs) m = noEff m { post_bodies = pwbs, current_time = t }


view :: Model -> View a
view m =
  div_
    []
    (
        [ h1_ [] [ text title ]
        , div_
            [ class_ "thread" ]
            (  (op_post thread_posts)
            ++ map (reply m backlinks) (drop 1 (post_bodies m))
            )
        ]
    )

    where
        thread_posts :: [ Post ]
        thread_posts =
            concatMap (Thread.posts) $
                concatMap (Board.threads) $
                    Site.boards (site m)

        backlinks :: Backlinks
        backlinks = collectBacklinks (post_bodies m)

        op_post :: [ Post ] -> [ View a ]
        op_post [] = [ h2_ [] [ "There's nothing here" ] ]
        op_post (x:_) = op m x backlinks

        title :: JSString
        title = toMisoString $ (Site.name $ site m) <> " /" <> board <> "/"

        board = Board.pathpart $ head $ Site.boards (site m)


op :: Model -> Post -> Backlinks -> [ View a ]
op m op_post backlinks =
    [ files_or_embed_view
    , div_
        (
            [ class_ "post op"
            , id_ $ toMisoString $ show $ Post.board_post_id op_post
            ] ++ multi op_post
        )
        [ intro site_ board thread op_post backlinks $ current_time m
        , div_
            [ class_ "body" ]
            (body $ post_bodies m)
        ]
    ]

    where
        files_or_embed_view :: View a
        files_or_embed_view =
          case (Post.embed op_post) of
            Just _ -> embed op_post
            Nothing -> files (media_root m) site_ op_post
        

        site_ :: Site
        site_ = site m

        board :: Board
        board = head $ Site.boards site_

        thread :: Thread
        thread = head $ Board.threads board

        body :: [ PostWithBody ] -> [ View a ]
        body [] = []
        body x = Body.render m $ snd $ head x


multi :: Post -> [ Attribute a ]
multi post
    | length (Post.attachments post) > 1 = [ class_ "multifile" ]
    | otherwise = []


reply :: Model -> Backlinks -> PostWithBody -> View a
reply m backlinks (post, parts) = div_
    [ class_ "postcontainer"
    , id_ $ toMisoString $ show $ Post.board_post_id post
    ]
    [ div_
        [ class_ "sidearrows" ]
        [ text ">>" ]
    , div_
        (
            [ class_ "post reply"
            ] ++ multi post
        )
        [ intro site_ board thread post backlinks $ current_time m
        , files_or_embed_view
        , div_
            [ class_ "body" ]
            (Body.render m parts)
        ]
    ]

    where
        files_or_embed_view :: View a
        files_or_embed_view =
          case (Post.embed post) of
            Just txt -> embed post
            Nothing -> files (media_root m) site_ post

        site_ :: Site
        site_ = site m

        board :: Board
        board = head $ Site.boards site_

        thread :: Thread
        thread = head $ Board.threads board

