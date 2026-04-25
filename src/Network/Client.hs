{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Network.Client
    ( Http.HttpActionResult
    , Http.HttpMethod (..)
    , Http.HttpResult (..)
    , Action (..)
    , Model (..)
    , update
    , app
    ) where

import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)
import Miso.JSON (ToJSON)
import Control.Monad.State (get, put)
import Miso
    ( withSink
    , Effect
    , Component
    , publish
    , io, io_
    , subscribe
    , consoleError
    , topic
    , Topic
    , consoleLog
    , mailParent
    )
import Miso.Html.Element (div_)
import qualified Miso as M
import Miso.String (MisoString, toMisoString)

import qualified Network.Http as Http
import Common.Network.ClientTypes
import Common.FrontEnd.Types (MessagesFromChildren (..))


awaitResult
    :: Http.HttpActionResult
    -> ReturnTopicName
    -> Effect parent Model Action
awaitResult (_, resultVar) returnTopicName =
    withSink $ \sink -> do
        void $ forkIO $ do
            result <- ReturnResult <$> takeMVar resultVar
            sink $ Publish returnTopicName result


update :: Action -> Effect parent Model Action
update Initialize = do
    subscribe clientInTopic OnMessage OnErrorMessage
    mailParent MsgClientMounted

update (Publish returnTopicName x) = io_ $ publish returnTopic x
    where
        returnTopic :: Topic MessageOut
        returnTopic = topic returnTopicName

update (Connect returnTopicName actionResult) =
    awaitResult actionResult returnTopicName

update (OnMessage (_, InitModel m)) = do
    io_ $ consoleLog $ "HttpClient - InitModel received! " <> (toMisoString $ show m)
    put m

update (OnMessage (sender, FetchLatest t)) = do
    model <- get

    let payload = Just $ FetchCatalogArgs
            { max_time = t
            , max_row_read = fetchCount model
            }

    pghttp_ model "/rpc/fetch_catalog" Http.POST payload sender

update (OnMessage (sender, GetThread GetThreadArgs {..})) = do
    model <- get

    pghttp_ model path Http.GET (Nothing :: Maybe ()) sender

    where
        path = "/sites?"
            <> "select=*,boards(*,threads(*,posts(*,attachments(*))))"
            <> "&name=eq." <> toMisoString website
            <> "&boards.pathpart=eq." <> toMisoString board_pathpart
            <> "&boards.threads.board_thread_id=eq." <> toMisoString (show board_thread_id)
            <> "&boards.threads.posts.order=board_post_id.asc"

update (OnMessage (sender, LoadAllSitesAndBoards)) = do
    model <- get
    pghttp_ model "/sites?select=*,boards(*)" Http.GET (Nothing :: Maybe ()) sender

update (OnMessage (sender, Search query)) = do
    model <- get

    pghttp_ model "/rpc/search_posts" Http.POST payload sender

    where
        payload = Just $ SearchPostsArgs
            { search_text = query
            , max_rows = 100
            }

update (OnMessage (sender, DeleteIllegalPost args)) =
    http_ "/admin_/delete_post" Http.POST (Just args) sender

update (OnErrorMessage msg) =
    io_ $ consoleError ("Client Message decode failure: " <> toMisoString msg)


pghttp_
    :: (ToJSON a)
    => Model
    -> MisoString
    -> Http.HttpMethod
    -> Maybe a
    -> ReturnTopicName
    -> Effect parent Model Action
pghttp_ m apiPath method payload sender = do
    io_$ consoleLog $ "HttpClient - sending Connect. pgApiRoot: " <> pgApiRoot m
    io $ Connect sender <$> Http.http
        (pgApiRoot m <> apiPath)
        method
        [("Content-Type", "application/json")]
        payload


http_
    :: (ToJSON a)
    => MisoString
    -> Http.HttpMethod
    -> Maybe a
    -> ReturnTopicName
    -> Effect parent Model Action
http_ url method payload sender =
    io $ Connect sender <$> Http.http
        url
        method
        [("Content-Type", "application/json")]
        payload


app :: Component parent Model Action
app = M.Component
    { M.model = Uninitialized
    , M.hydrateModel = Nothing
    , M.update = update
    , M.view = const $ div_ [] []
    , M.subs = []
    , M.styles = []
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    , M.bindings = []
    , M.eventPropagation = False
    , M.mount = Just Initialize
    , M.unmount = Nothing
    }
