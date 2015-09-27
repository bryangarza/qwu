module Qwu.DB.Util where

import Qwu.DB.Connect

import Crypto.PasswordStore (makePassword)
import Data.Int (Int64)
import Data.ByteString (ByteString)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Database.PostgreSQL.Simple (Connection, close)

genUuid :: IO UUID
genUuid = nextRandom

genPassword :: ByteString -> IO ByteString
genPassword x = makePassword x 18

runWithConn :: (Connection -> t2 -> t3 -> IO Int64) -> t2 -> t3 -> IO ()
runWithConn action x y =
  do
    conn <- pConnect
    action conn x y
    close conn

runWithConn3 :: (Connection -> t2 -> t3 -> t4 -> IO Int64) -> t2 -> t3 -> t4 -> IO ()
runWithConn3 action x y z =
  do
    conn <- pConnect
    action conn x y z
    close conn
