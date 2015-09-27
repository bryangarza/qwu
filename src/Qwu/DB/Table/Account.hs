{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Qwu.DB.Table.Account where

import GHC.Generics
import Data.Aeson
import Data.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text
import Data.UUID as U
import Data.UUID.Aeson
import Opaleye
  ( Column
  , optional
  , PGText
  , PGUuid
  , required
  , Table(Table) )

type AccountId = UUID
type Username  = Text
type Email     = Text
type Password  = Text

data Account' a b c d = Account
    { accountId :: a
    , username  :: b
    , email     :: c
    , password  :: d
    } deriving (Eq, Show, Generic)

type Account = Account' AccountId Username Email Password

instance ToJSON Account

instance Default Account where
  def = Account U.nil "foo" "foo@bar.com" "quux123456"

$(makeAdaptorAndInstance "pAccount" ''Account')

-- type signature format:
-- someTable :: Table <writes>
--                    <reads>

type ColumnW = Account' (Column PGUuid) (Column PGText) (Column PGText) (Column PGText)
type ColumnR = Account' (Column PGUuid) (Column PGText) (Column PGText) (Column PGText)

table :: Table ColumnW ColumnR
table = Table "accountTable" (
  pAccount Account { accountId = required "accountId"
                   , username  = required "username"
                   , email     = required "email"
                   , password  = required "password" })
