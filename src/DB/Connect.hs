{-# LANGUAGE OverloadedStrings #-}
module DB.Connect (pConnect) where

import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative

myConnectInfo :: ConnectInfo
myConnectInfo = ConnectInfo
  { connectHost     = "127.0.0.1"
  , connectPort     = 5432
  , connectUser     = "bryangarza"
  , connectPassword = ""
  , connectDatabase = "qwu"
  }

pConnect :: IO Connection
pConnect = connect myConnectInfo
