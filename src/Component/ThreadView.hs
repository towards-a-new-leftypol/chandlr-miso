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

import Data.Maybe (catMaybes)
import Miso.String (toMisoString)
import GHCJS.DOM.Types (JSString)

import Network.SiteType (Site)
import qualified Network.SiteType as Site
import Network.PostType (Post)
import qualified Network.PostType as Post
import qualified Network.BoardType as Board
import qualified Network.ThreadType as Thread
import Component.Thread.Files (files)
import Component.Thread.Intro (intro)
import Component.Thread.Model
import BodyParser
import qualified Component.BodyRender as Body

initialModel :: JSString -> Site -> Model
initialModel mroot s = Model
    { site = s
    , post_bodies = []
    , media_root = mroot
    }

data Action
    = RenderSite Site
    | UpdatePostBodies [ PostWithBody ]

data Interface a = Interface { passAction :: Action -> a }

update :: Interface a -> Action -> Model -> Effect a Model
update iface (RenderSite s) m = m { site = s } <# do
    bodies <- mapM parsePostBody (catMaybes $ map Post.body posts)

    mapM_ (consoleLog . toMisoString . show) bodies

    return $ passAction iface $ UpdatePostBodies $ zip posts bodies

    where
        posts :: [ Post ]
        posts = Thread.posts $ head $ Board.threads $ head $ Site.boards s
--update (RenderSite s) m = noEff (m { site = s })

update _ (UpdatePostBodies pwbs) m = noEff m { post_bodies = pwbs }


view :: Model -> View a
view m =
  div_
    []
    (
        [ h1_ [] [ text title ]
        , div_
            [ class_ "thread" ]
            (  (op_post thread_posts)
            ++ map (reply m) (drop 1 (post_bodies m))
            )
        ]
    )

    where
        thread_posts :: [ Post ]
        thread_posts =
            concatMap (Thread.posts) $
                concatMap (Board.threads) $
                    Site.boards (site m)

        op_post :: [ Post ] -> [ View a ]
        op_post [] = [ h2_ [] [ "There's nothing here" ] ]
        op_post (x:_) = op m x

        title :: JSString
        title = toMisoString $ (Site.name $ site m) <> " /" <> board <> "/"

        board = Board.pathpart $ head $ Site.boards (site m)


op :: Model -> Post -> [ View a ]
op m op_post =
    [ files (media_root m) (site m) op_post
    , div_
        (
            [ class_ "post op"
            , id_ $ toMisoString $ show $ Post.board_post_id op_post
            ] ++ multi op_post
        )
        [ intro op_post
        , div_
            [ class_ "body" ]
            (body $ post_bodies m)
        ]
    ]

    where
        body :: [ PostWithBody ] -> [ View a ]
        body [] = []
        body x = Body.render m $ snd $ head x


multi :: Post -> [ Attribute a ]
multi post
    | length (Post.attachments post) > 1 = [ class_ "multifile" ]
    | otherwise = []


reply :: Model -> PostWithBody -> View a
reply m (post, parts) = div_
    [ class_ "postcontainer"
    , id_ $ toMisoString $ show $ Post.board_post_id post
    ]
    [ div_
        [ class_ "sidearrows" ]
        [ text ">>" ]
    , div_
        (
            [ class_ "post reply"
            , id_ "reply_477702"
            ] ++ multi post
        )
        [ intro post
        , files (media_root m) (site m) post
        , div_
            [ class_ "body" ]
            (Body.render m parts)
        ]
    ]
