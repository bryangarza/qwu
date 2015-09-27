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

import qualified DB.Util as Util
import qualified DB.Table.Post         as Post
import qualified DB.Table.Account      as Account
import DB.Table.Post     (Post, Post'(Post))
import DB.Table.Account  (Account, Account'(Account))

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Clock as Clock
import qualified Data.UUID as U
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Opaleye ((.==), runInsert, runDelete, runInsertReturning, runUpdate)
import Opaleye.PGTypes (pgStrictText, pgInt4, pgUUID, pgUTCTime)
import Opaleye.Column (Column)
import Opaleye.Internal.Table (Table)

createPost :: Post -> Account.AccountId -> IO ()
createPost Post {Post.body} accountId =
  do
    timestamp <- Clock.getCurrentTime
    Util.runWithConn runInsert Post.table (columns timestamp)
  where
    body'                  = pgStrictText body
    accountId'             = pgUUID accountId
    columns t              = Post Nothing body' (pgUTCTime t) accountId'

updatePostField :: (Post.ColumnW -> Post.ColumnW) -> Post.PostId -> IO ()
updatePostField update idToMatch =
  Util.runWithConn3 runUpdate Post.table update' match
  where
    update' x@Post {Post.postId} = update (x { Post.postId = Just postId })
    match Post {Post.postId} = postId .== pgInt4 idToMatch

updatePostBody :: Post.Body -> Post.PostId -> IO ()
updatePostBody newBody = updatePostField update
  where
    newBody' = pgStrictText newBody
    update x = x { Post.body = newBody' }

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

updateAccountField :: (Account.ColumnR -> Account.ColumnW) -> Account.AccountId -> IO ()
updateAccountField update idToMatch =
  Util.runWithConn3 runUpdate Account.table update match
  where
    match Account {Account.accountId} = accountId .== pgUUID idToMatch

updateAccountUsername :: Account.Username -> Account.AccountId -> IO ()
updateAccountUsername newUsername = updateAccountField update
  where newUsername' = pgStrictText newUsername
        update x = x { Account.username = newUsername' }

updateAccountEmail :: Account.Email -> Account.AccountId -> IO ()
updateAccountEmail newEmail = updateAccountField update
  where
    newEmail' = pgStrictText newEmail
    update x = x { Account.email = newEmail' }

updateAccountPassword :: Account.Password -> Account.AccountId -> IO ()
updateAccountPassword newPassword accountId =
  do
    hash <- Util.genPassword (encodeUtf8 newPassword)
    updateAccountField (update hash) accountId
  where
    update :: BS.ByteString -> Account.ColumnR -> Account.ColumnW
    update hash x = x { Account.password = hash' }
      where hash' = pgStrictText (decodeUtf8 hash)

deletePost :: Post.PostId -> IO ()
deletePost idToMatch = Util.runWithConn runDelete Post.table match
  where
    match Post {Post.postId} = postId .== pgInt4 idToMatch

deleteAccount :: Account.AccountId -> IO ()
deleteAccount idToMatch = Util.runWithConn runDelete Account.table match
  where
    match Account {Account.accountId} = accountId .== pgUUID idToMatch
