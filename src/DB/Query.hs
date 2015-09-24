{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module DB.Query where

import DB.Connect
import DB.Table.Post as P

import qualified Data.UUID as U

import Control.Arrow (returnA, (<<<))
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection)
import Opaleye.PGTypes (pgUUID)
import Opaleye ( Query
               , restrict
               , (.==)
               , queryTable
               , runQuery
               , showSqlForPostgres
               , Unpackspec )

postQuery :: Query P.ColumnR
postQuery = queryTable P.table

postByAccountId :: U.UUID -> Query P.ColumnR
postByAccountId id_ = proc () -> do
  row@Post{accountId} <- postQuery -< ()
  restrict -< accountId .== pgUUID id_
  returnA -< row

runPostByAccountId :: IO [P.Post]
runPostByAccountId =
  do
    conn <- pConnect
    runPostByAccountId' conn (postByAccountId U.nil)
      where runPostByAccountId' :: Connection -> Query P.ColumnR -> IO [P.Post]
            runPostByAccountId' = runQuery

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres
