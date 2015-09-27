{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.Post where

import DB.Table.Post
import DB.Table.Post as P
import Html.Base

import Data.Text(pack)
import Lucid

-- HTML serialization of a single post
instance ToHtml Post where
  toHtml post =
    li_ $ do
      div_ [class_ "post", id_ (pack . show $ postId post)] $ do
        div_ [class_ "accountId"] (toHtml . show $ accountId post)
        div_ [class_ "timestamp"] (toHtml . show $ ts post)
        div_ [class_ "body"] (toHtml $ body post)

  toHtmlRaw = toHtml

-- HTML serialization of a list of posts
instance ToHtml [Post] where
  toHtml posts = baseHtml . ul_ $ do
    foldMap toHtml posts

  toHtmlRaw = toHtml
