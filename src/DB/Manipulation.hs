{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Manipulation (
    createPost
  , createAccount
  , deletePost
  , deleteAccount
  , updatePostBody
  , updateAccountUsername
  , updateAccountEmail
  , updateAccountPassword
  ) where

import DB.Connect
import qualified DB.Util as Util
import DB.Table.Post     (Post, Post'(Post), body, ts)
import DB.Table.Account  (Account, Account'(Account), username, email, password)
import qualified DB.Table.Post         as Post
import qualified DB.Table.Account      as Account
-- import qualified DB.Table.Relationship as Rel

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Clock as Clock
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection, close)
import Opaleye ((.==), runInsert, runDelete, runInsertReturning, runUpdate)
import Opaleye.PGTypes (pgStrictText, pgInt4, pgUUID, pgUTCTime)
import Opaleye.Column (Column)
import Opaleye.Internal.Table (Table)

createPost :: Post -> U.UUID -> IO ()
createPost Post {body} accountId =
  do
    timestamp <- Clock.getCurrentTime
    Util.runWithConn runInsert Post.table (columns timestamp)
  where
    body'                  = pgStrictText body
    accountId'             = pgUUID accountId
    columns t              = Post Nothing body' (pgUTCTime t) accountId'
    select Post {Post.id_} = id_

updatePostField :: (Post.ColumnW -> Post.ColumnW) -> Post.Id_ -> IO ()
updatePostField update idToMatch =
  Util.runWithConn3 runUpdate Post.table update' match
  where
    update' x@Post {Post.id_} = update (x { Post.id_ = Just id_ })
    match Post {Post.id_} = id_ .== pgInt4 idToMatch

updatePostBody :: Post.Body -> Post.Id_ -> IO ()
updatePostBody newBody = updatePostField update
  where
    newBody' = pgStrictText newBody
    update x = x { body = newBody' }

createAccount :: Account -> IO ()
createAccount (Account accountId username email password) =
  do
    accountId <- Util.genUuid
    hash      <- Util.genPassword (encodeUtf8 password)
    Util.runWithConn runInsert Account.table (columns hash)
  where
    accountId' = pgUUID accountId
    username' = pgStrictText username
    email' = pgStrictText email
    columns hash = Account accountId' username' email' hash'
      where hash' = pgStrictText (decodeUtf8 hash)

updateAccountField :: (Account.ColumnR -> Account.ColumnW) -> Account.Id_ -> IO ()
updateAccountField update idToMatch =
  Util.runWithConn3 runUpdate Account.table update match
  where
    match Account {Account.id_} = id_ .== pgUUID idToMatch

updateAccountUsername :: Account.Username -> Account.Id_ -> IO ()
updateAccountUsername newUsername = updateAccountField update
  where newUsername' = pgStrictText newUsername
        update x = x { username = newUsername' }

updateAccountEmail :: Account.Email -> Account.Id_ -> IO ()
updateAccountEmail newEmail = updateAccountField update
  where
    newEmail' = pgStrictText newEmail
    update x = x { email = newEmail' }

updateAccountPassword :: Account.Password -> Account.Id_ -> IO ()
updateAccountPassword newPassword id_ =
  do
    hash <- Util.genPassword (encodeUtf8 newPassword)
    updateAccountField (update hash) id_
  where
    update :: BS.ByteString -> Account.ColumnR -> Account.ColumnW
    update hash x = x { password = hash' }
      where hash' = pgStrictText (decodeUtf8 hash)

deletePost :: Post.Id_ -> IO ()
deletePost idToMatch = Util.runWithConn runDelete Post.table match
  where
    match Post {Post.id_} = id_ .== pgInt4 idToMatch

deleteAccount :: Account.Id_ -> IO ()
deleteAccount idToMatch = Util.runWithConn runDelete Account.table match
  where
    match Account {Account.id_} = id_ .== pgUUID idToMatch
