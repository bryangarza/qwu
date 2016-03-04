{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}

module Qwu.DB.Query where

import Qwu.DB.Connect
import Qwu.DB.Table.Account (AccountId)
import Qwu.DB.Table.Post

import qualified Data.UUID as U
import Data.UUID
import Control.Lens                    (view)
import Control.Arrow                   (returnA)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple      (Connection)
import Opaleye.PGTypes                 (pgUUID)
import Opaleye
  ( Query
  , restrict
  , (.==)
  , queryTable
  , runQuery
  , showSqlForPostgres
  , Unpackspec )

postQuery :: Query ColumnR
postQuery = queryTable table

postByAccountId :: AccountId -> Query ColumnR
postByAccountId idToMatch = proc () -> do
  row <- postQuery -< ()
  restrict -< (view accountId row) .== pgUUID idToMatch
  returnA -< row

runPostByAccountId :: IO [Post]
runPostByAccountId =
  do
    conn <- pConnect
    runPostByAccountId' conn (postByAccountId U.nil)
      where runPostByAccountId' :: Connection -> Query ColumnR -> IO [Post]
            runPostByAccountId' = runQuery

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres
