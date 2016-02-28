module Qwu.DB.Manipulation (
    createPost
  , createAccount
  , deletePost
  , deleteAccount
  , updatePostBody
  , updateAccountUsername
  , updateAccountEmail
  , updateAccountPassword
  ) where

import           Qwu.DB.Table.Post
import qualified Qwu.DB.Table.Post      as Post
import           Qwu.DB.Table.Account
import qualified Qwu.DB.Table.Account   as Account
import           Qwu.DB.Util

import           Control.Lens       (set, view)
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
createPost p accountId =
  do
    timestamp <- getCurrentTime
    runWithConn runInsert Post.table (columns timestamp)
  where
    body'      = pgStrictText (view body p)
    accountId' = pgUUID accountId
    columns t  = Post Nothing body' (pgUTCTime t) accountId'

updatePostField :: (Post.ColumnW -> Post.ColumnW) -> PostId -> IO ()
updatePostField update idToMatch =
  runWithConn3 runUpdate Post.table update' match
  where
    idToMatch' = pgInt4 idToMatch
    update' = update . set postId (Just idToMatch')
    match = (.== idToMatch') . view postId

updatePostBody :: Body -> PostId -> IO ()
updatePostBody = updatePostField . set body . pgStrictText

deletePost :: PostId -> IO ()
deletePost idToMatch = runWithConn runDelete Post.table match
  where
    match = (.== pgInt4 idToMatch) . view postId

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
    match = (.== pgUUID idToMatch) . view Account.accountId

updateAccountUsername :: Username -> AccountId -> IO ()
updateAccountUsername = updateAccountField . set username . pgStrictText

updateAccountEmail :: Email -> AccountId -> IO ()
updateAccountEmail = updateAccountField . set email . pgStrictText

updateAccountPassword :: Password -> AccountId -> IO ()
updateAccountPassword newPassword accountId =
  do
    hash <- genPassword (encodeUtf8 newPassword)
    updateAccountField (update hash) accountId
  where
    update :: ByteString -> Account.ColumnR -> Account.ColumnW
    update = set password . pgStrictText . decodeUtf8

deleteAccount :: AccountId -> IO ()
deleteAccount idToMatch = runWithConn runDelete Account.table match
  where
    match = (.== pgUUID idToMatch) . view Account.accountId
