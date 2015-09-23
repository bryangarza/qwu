{-# LANGUAGE FlexibleContexts #-}
module DB.Query where

import DB.Table.Post as P

import Data.Profunctor.Product.Default (Default)
import Opaleye ( Query
               , queryTable
               , showSqlForPostgres
               , Unpackspec )

postQuery :: Query P.ColumnR
postQuery = queryTable P.table

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres
