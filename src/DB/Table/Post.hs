{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module DB.Table.Post where

import DB.Table.Account (AccountId)
import qualified Data.Time as Time

import Data.Aeson
import Data.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text
import Data.UUID as U
import Data.UUID.Aeson
import GHC.Generics
import Opaleye ( Column
               , Table(Table)
               , required
               , optional
               , PGInt4
               , PGText
               , PGTimestamptz
               , PGUuid )

type PostId    = Int
type Body      = Text
type Ts        = Time.UTCTime

data Post' a b c d = Post
    { postId    :: a
    , body      :: b
    , ts        :: c
    , accountId :: d
    } deriving (Eq, Show, Generic)

type Post = Post' PostId Body Ts AccountId

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
  pPost Post { postId    = optional "postId"
             , body      = required "body"
             , ts        = required "ts"
             , accountId = required "accountId" })
