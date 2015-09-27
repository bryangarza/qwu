module Qwu.DB.Connect (pConnect) where

import Database.PostgreSQL.Simple

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
