module Component.ThreadView
( Model
) where

import Common.PostsType (Post)

data Model = Model
  { thread_id :: Integer
  , thread_posts :: [ Post ]
  , thread_loading :: Bool
  } deriving Eq
