{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Network.Client
    ( Http.HttpActionResult
    , Http.HttpMethod (..)
    , Http.HttpResult (..)
    , Action (..)
    , Model
    , Props (..)
    , update
    , app
    ) where

import Control.Monad (void)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (takeMVar)
import Miso.JSON (ToJSON)
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
    , vfrag
    , getProps
    , get
    , modify
    )
import qualified Miso as M
import Miso.String (MisoString, toMisoString)

import qualified Network.Http as Http
import Common.Network.ClientTypes
import Common.FrontEnd.Types (MessagesFromChildren (..))


awaitResult
    :: Http.HttpActionResult
    -> ReturnTopicName
    -> Effect parent Props Model Action
awaitResult (_, resultVar) returnTopicName =
    withSink $ \sink -> do
        void $ forkIO $ do
            result <- ReturnResult <$> takeMVar resultVar
            sink $ Publish returnTopicName result


update :: Action -> Effect parent Props Model Action
update Initialize = do
    subscribe clientInTopic OnMessage OnErrorMessage
    mailParent MsgClientMounted
    io_ $ consoleLog "Network.Client ready."

update PropsInitialized = do
    model <- get
    mapM_ (uncurry onMessage) (messageQueue model)
    modify $ \m -> m { initialized = True, messageQueue = [] }

update (Publish returnTopicName x) = io_ $ publish returnTopic x
    where
        returnTopic :: Topic MessageOut
        returnTopic = topic returnTopicName

update (Connect returnTopicName actionResult) =
    awaitResult actionResult returnTopicName

update (OnMessage msgIn) = do
    model <- get

    if initialized model
    then
        uncurry onMessage msgIn
    else
        modify $ \m -> m { messageQueue = messageQueue m ++ [ msgIn ] }

update (OnErrorMessage msg) =
    io_ $ consoleError ("Client Message decode failure: " <> toMisoString msg)

onMessage :: ReturnTopicName -> Query -> Effect context Props Model Action
onMessage sender (FetchLatest args) =
    pghttp_ "/rpc/fetch_catalog2" Http.POST (Just args) sender

onMessage sender (GetThread GetThreadArgs {..}) = do
    pghttp_ path Http.GET (Nothing :: Maybe ()) sender

    where
        path = "/sites?"
            <> "select=*,boards(*,threads(*,posts(*,attachments(*))))"
            <> "&name=eq." <> toMisoString website
            <> "&boards.pathpart=eq." <> toMisoString board_pathpart
            <> "&boards.threads.board_thread_id=eq." <> toMisoString (show board_thread_id)
            <> "&boards.threads.posts.order=board_post_id.asc"

onMessage sender LoadAllSitesAndBoards = do
    pghttp_ "/sites?select=*,boards(*)" Http.GET (Nothing :: Maybe ()) sender

onMessage sender (Search query) = do
    pghttp_ "/rpc/search_posts" Http.POST payload sender

    where
        payload = Just $ SearchPostsArgs
            { search_text = query
            , max_rows = 100
            }

onMessage sender (DeleteIllegalPost args) =
    http_ "/admin_/delete_post" Http.POST (Just args) sender



pghttp_
    :: (ToJSON a)
    => MisoString
    -> Http.HttpMethod
    -> Maybe a
    -> ReturnTopicName
    -> Effect parent Props Model Action
pghttp_ apiPath method payload sender = do
    props <- getProps

    io $ do
        consoleLog $ "HttpClient - sending Connect. pgApiRoot: " <> pgApiRoot props
        Connect sender <$> Http.http
            (pgApiRoot props <> apiPath)
            method
            [("Content-Type", "application/json")]
            payload


http_
    :: (ToJSON a)
    => MisoString
    -> Http.HttpMethod
    -> Maybe a
    -> ReturnTopicName
    -> Effect parent props Model Action
http_ url method payload sender =
    io $ Connect sender <$> Http.http
        url
        method
        [("Content-Type", "application/json")]
        payload


app :: Component context Props Model Action
app = M.Component
    { M.model = Model { initialized = False, messageQueue = [] }
    , M.hydrateModel = Nothing
    , M.update = update
    , M.view = const $ const $ const $ vfrag []
    , M.subs = []
    , M.styles = []
    , M.mountPoint = Nothing
    , M.logLevel = M.DebugAll
    , M.scripts = []
    , M.mailbox = const Nothing
    , M.eventPropagation = False
    , M.mount = Just Initialize
    , M.unmount = Nothing
    , M.onPropsChanged = Just $ const $ const PropsInitialized
    , M.useContext = False
    }
