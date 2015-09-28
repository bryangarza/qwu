{-# LANGUAGE OverloadedStrings #-}

module Qwu.Html.NewPost where

import Control.Monad
import Lucid

newPost :: Monad m => HtmlT m ()
newPost =
  form_ [action_ "/newpost", method_ "post"]
    (div_ [class_ "newPostForm"]
     (do (textarea_ [name_ "msg"
                    ,rows_ "10"
                    ,cols_ "50"]
          "Write a new post")
         (div_ [class_ "button"]
          (button_ [type_ "submit"] "Submit!"))))
