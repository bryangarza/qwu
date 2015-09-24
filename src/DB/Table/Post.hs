{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module DB.Table.Post where

import Data.Aeson
import Data.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text
import Data.UUID as U
import Data.UUID.Aeson
import GHC.Generics
import Lucid ( ToHtml
             , toHtml
             , toHtmlRaw
             , table_
             , tr_
             , td_
             , th_
             )
import Opaleye ( Column
               , Table(Table)
               , required
               , optional
               , PGInt4
               , PGText
               , PGTimestamptz
               , PGUuid )
import qualified Data.Time as Time

type Id_       = Int
type Body      = Text
type Ts        = Time.UTCTime
type AccountId = UUID

data Post' a b c d = Post
    { id_       :: a
    , body      :: b
    , ts        :: c
    , accountId :: d
    } deriving (Eq, Show, Generic)

type Post = Post' Id_ Body Ts AccountId

instance ToJSON Post

instance Default Post where
  def = Post 0 "a post body" timestamp U.nil
    where
      Just timestamp =
        Time.parseTimeM True Time.defaultTimeLocale "%c" "Thu Jan  1 00:00:10 UTC 1970" :: Maybe Time.UTCTime

$(makeAdaptorAndInstance "pPost" ''Post')

-- type signature format:
-- someTable :: Table <writes>
--                    <reads>

type ColumnW = Post' (Maybe (Column PGInt4)) (Column PGText) (Column PGTimestamptz) (Column PGUuid)
type ColumnR = Post'        (Column PGInt4)  (Column PGText) (Column PGTimestamptz) (Column PGUuid)

table :: Table ColumnW ColumnR
table = Table "postTable" (
  pPost Post { id_       = optional "postId"
             , body      = required "body"
             , ts        = required "ts"
             , accountId = required "accountId" })


-- HTML serialization of a single person
instance ToHtml Post where
  toHtml post =
    tr_ $ do
      td_ (toHtml . show $ id_ post)
      td_ (toHtml $ body post)
      td_ (toHtml . show $ ts post)
      td_ (toHtml . show $ accountId post)

  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Post] where
  toHtml posts = table_ $ do
    tr_ $ do
      th_ "id"
      th_ "content"
      th_ "timestamp"
      th_ "account id"

    foldMap toHtml posts

  toHtmlRaw = toHtml
