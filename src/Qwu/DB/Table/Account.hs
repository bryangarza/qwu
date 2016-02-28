{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Qwu.DB.Table.Account where

import GHC.Generics
import Control.Lens (makeLenses)
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
    { _accountId :: a
    , _username  :: b
    , _email     :: c
    , _password  :: d
    } deriving (Eq, Show, Generic)

makeLenses ''Account'

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
  pAccount Account { _accountId = required "accountId"
                   , _username  = required "username"
                   , _email     = required "email"
                   , _password  = required "password" })
