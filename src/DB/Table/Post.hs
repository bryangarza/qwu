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
  def = Post 0 "a post body" ts U.nil
    where
-- TODO: fix
-- src/DB/Table/Post.hs:41:9-22: Warning: …
--     In the use of ‘Time.parseTime’
--     (imported from Data.Time, but defined in time-1.5.0.1:Data.Time.Format.Parse):
--     Deprecated: "use "parseTimeM True" instead"
      Just ts =
        Time.parseTime Time.defaultTimeLocale "%c" "Thu Jan  1 00:00:10 UTC 1970" :: Maybe Time.UTCTime

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
