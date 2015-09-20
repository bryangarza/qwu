{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module DB.Util where

import DB.Connect

import qualified Crypto.PasswordStore as PwS
import Data.Int (Int64)
import qualified Data.ByteString as BS
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import Database.PostgreSQL.Simple (Connection, close)
import Opaleye ((.==))
import Opaleye.PGTypes (PGBool)
import Opaleye.Column (Column)
import Opaleye.Internal.Table (Table)

genUuid :: IO U.UUID
genUuid = U4.nextRandom

genPassword :: BS.ByteString -> IO BS.ByteString
genPassword x = PwS.makePassword x 18

runWithConn :: (Connection -> t2 -> t3 -> IO Int64) -> (t2 -> t3 -> IO ())
runWithConn action x y =
  do
    conn <- pConnect
    action conn x y
    close conn

runWithConn3 :: (Connection -> t2 -> t3 -> t4 -> IO Int64) -> (t2 -> t3 -> t4 -> IO ())
runWithConn3 action x y z =
  do
    conn <- pConnect
    action conn x y z
    close conn

class MatchId table idToMatch row where
  matchId :: table -> idToMatch -> row -> Column PGBool

instance MatchId (Table (a1,a2,a3) (Column b1,b2,b3)) (Column b1) (Column b1,b2,b3) where
  matchId _ idToMatch = \(id_,_,_) -> id_ .== idToMatch

instance MatchId (Table (a1,a2,a3,a4) (Column b1,b2,b3,b4)) (Column b1) (Column b1,b2,b3,b4) where
  matchId _ idToMatch = \(id_,_,_,_) -> id_ .== idToMatch
