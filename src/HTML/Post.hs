{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.Post where

import DB.Table.Post hiding (id_) -- conflicts with Lucid
import DB.Table.Post as P
import Html.Base

import Lucid

-- HTML serialization of a single post
instance ToHtml Post where
  toHtml post =
    tr_ $ do
      td_ (toHtml . show $ P.id_ post)
      td_ (toHtml $ body post)
      td_ (toHtml . show $ ts post)
      td_ (toHtml . show $ accountId post)

  toHtmlRaw = toHtml

-- HTML serialization of a list of posts
instance ToHtml [Post] where
  toHtml posts = demo . table_ $ do
    tr_ $ do
      th_ "id"
      th_ "content"
      th_ "timestamp"
      th_ "account id"

    foldMap toHtml posts

  toHtmlRaw = toHtml
