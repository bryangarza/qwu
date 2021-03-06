{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Qwu.DB.Table.Post where

import Qwu.DB.Table.Account (AccountId)

import GHC.Generics
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text
import Data.Time (defaultTimeLocale, parseTimeM, UTCTime)
import Data.UUID as U
import Data.UUID.Aeson
import Opaleye
  ( Column
  , optional
  , required
  , PGInt4
  , PGText
  , PGTimestamptz
  , PGUuid
  , Table(Table) )

type PostId    = Int
type Body      = Text
type Ts        = UTCTime

data Post' a b c d = Post
    { _postId    :: a
    , _body      :: b
    , _ts        :: c
    , _accountId :: d
    } deriving (Eq, Show, Generic)

makeLenses ''Post'

type Post = Post' PostId Body Ts AccountId

instance ToJSON Post

instance Default Post where
  def = Post 0 "a post body" timestamp U.nil
    where
      Just timestamp =
        parseTimeM True defaultTimeLocale "%c" "Thu Jan  1 00:00:10 UTC 1970" :: Maybe UTCTime

$(makeAdaptorAndInstance "pPost" ''Post')

-- type signature format:
-- someTable :: Table <writes>
--                    <reads>

type ColumnW = Post' (Maybe (Column PGInt4)) (Column PGText) (Column PGTimestamptz) (Column PGUuid)
type ColumnR = Post'        (Column PGInt4)  (Column PGText) (Column PGTimestamptz) (Column PGUuid)

table :: Table ColumnW ColumnR
table = Table "postTable" (
  pPost Post { _postId    = optional "postId"
             , _body      = required "body"
             , _ts        = required "ts"
             , _accountId = required "accountId" })
