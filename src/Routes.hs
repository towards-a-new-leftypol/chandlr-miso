{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Miso (View)

import Servant.API

import Action

type Route
    =    R_Latest
    :<|> R_Thread

type R_Latest = View Action

-- Show selected thread
--  - <website>/<board>/res/<thread_id>
type R_Thread
    =  Capture "website" String
    :> Capture "board"   String 
    :> "res"
    :> Capture "board_thread_id" BoardThreadId
    :> View Action

type BoardThreadId = Int
