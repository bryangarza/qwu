{-# LANGUAGE NamedFieldPuns #-}

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

import           DB.Table.Post
import qualified DB.Table.Post      as Post
import           DB.Table.Account
import qualified DB.Table.Account   as Account
import           DB.Util

import           Data.ByteString    (ByteString)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Time.Clock    (getCurrentTime)
import Opaleye
  ( (.==)
  , runDelete
  , runInsert
  , runUpdate
  )
import Opaleye.PGTypes
  ( pgStrictText
  , pgInt4
  , pgUUID
  , pgUTCTime )

createPost :: Post -> AccountId -> IO ()
createPost Post {body} accountId =
  do
    timestamp <- getCurrentTime
    runWithConn runInsert Post.table (columns timestamp)
  where
    body'                  = pgStrictText body
    accountId'             = pgUUID accountId
    columns t              = Post Nothing body' (pgUTCTime t) accountId'

updatePostField :: (Post.ColumnW -> Post.ColumnW) -> PostId -> IO ()
updatePostField update idToMatch =
  runWithConn3 runUpdate Post.table update' match
  where
    update' x@Post {postId} = update (x { postId = Just postId })
    match Post {postId} = postId .== pgInt4 idToMatch

updatePostBody :: Body -> PostId -> IO ()
updatePostBody newBody = updatePostField update
  where
    newBody' = pgStrictText newBody
    update x = x { body = newBody' }

createAccount :: Account -> IO ()
createAccount (Account accountId username email password) =
  do
    accountId <- genUuid
    hash      <- genPassword (encodeUtf8 password)
    runWithConn runInsert Account.table (columns hash)
  where
    accountId' = pgUUID accountId
    username' = pgStrictText username
    email' = pgStrictText email
    columns hash = Account accountId' username' email' hash'
      where hash' = pgStrictText (decodeUtf8 hash)

updateAccountField :: (Account.ColumnR -> Account.ColumnW) -> AccountId -> IO ()
updateAccountField update idToMatch =
  runWithConn3 runUpdate Account.table update match
  where
    match Account {Account.accountId} = accountId .== pgUUID idToMatch

updateAccountUsername :: Username -> AccountId -> IO ()
updateAccountUsername newUsername = updateAccountField update
  where newUsername' = pgStrictText newUsername
        update x = x { username = newUsername' }

updateAccountEmail :: Email -> AccountId -> IO ()
updateAccountEmail newEmail = updateAccountField update
  where
    newEmail' = pgStrictText newEmail
    update x = x { email = newEmail' }

updateAccountPassword :: Password -> AccountId -> IO ()
updateAccountPassword newPassword accountId =
  do
    hash <- genPassword (encodeUtf8 newPassword)
    updateAccountField (update hash) accountId
  where
    update :: ByteString -> Account.ColumnR -> Account.ColumnW
    update hash x = x { password = hash' }
      where hash' = pgStrictText (decodeUtf8 hash)

deletePost :: PostId -> IO ()
deletePost idToMatch = runWithConn runDelete Post.table match
  where
    match Post {postId} = postId .== pgInt4 idToMatch

deleteAccount :: AccountId -> IO ()
deleteAccount idToMatch = runWithConn runDelete Account.table match
  where
    match Account {Account.accountId} = accountId .== pgUUID idToMatch
