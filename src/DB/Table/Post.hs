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
import GHC.Generics
import Opaleye ( Column
               , Table(Table)
               , required
               , optional
               , PGInt4
               , PGText
               , PGTimestamp )
import qualified Data.Time as Time

type Id_   = Int
type Body  = Text
type Ts    = Time.UTCTime

data Post' a b c = Post
    { id_  :: a
    , body :: b
    , ts   :: c
    } deriving (Eq, Show, Generic)

type Post = Post' Id_ Body Ts

instance ToJSON Post

-- instance Default Card where
--   def = Post 0 "a post body" <UTC TIME>

$(makeAdaptorAndInstance "pPost" ''Post')

-- type signature format:
-- someTable :: Table <writes>
--                    <reads>

type ColumnW = Post' (Maybe (Column PGInt4)) (Column PGText) (Column PGTimestamp)
type ColumnR = Post'        (Column PGInt4)  (Column PGText) (Column PGTimestamp)

table :: Table ColumnW ColumnR
table = Table "postTable" (
  pPost Post { id_   = optional "postId"
             , body  = required "body"
             , ts    = required "ts" })
