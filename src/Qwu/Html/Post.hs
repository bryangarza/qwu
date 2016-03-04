{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Qwu.Html.Post where

import Qwu.DB.Table.Post
import Qwu.Html.Base

import Control.Lens (view)
import Data.Text (pack)
import Lucid

-- HTML serialization of a single post
instance ToHtml Post where
  toHtml post =
    li_ $ do
      div_ [class_ "post", id_ (pack . show $ view postId post)] $ do
        div_ [class_ "accountId"] (toHtml . show $ view accountId post)
        div_ [class_ "timestamp"] (toHtml . show $ view ts post)
        div_ [class_ "body"] (toHtml $ view body post)

  toHtmlRaw = toHtml

-- HTML serialization of a list of posts
instance ToHtml [Post] where
  toHtml posts = baseHtml . ul_ $ do
    foldMap toHtml posts

  toHtmlRaw = toHtml
