{-# LANGUAGE OverloadedStrings #-}
module DB.Test where

import DB.Manipulation
import DB.Table.Account
import DB.Table.Post

import Data.Default
import Data.Text
import Data.UUID as U

testCreatePost :: IO ()
testCreatePost = createPost (def :: Post) U.nil

testUpdatePostBody :: PostId -> IO ()
testUpdatePostBody = updatePostBody "updated"

testUpdateAccountUsername :: IO ()
testUpdateAccountUsername = updateAccountUsername "updatedUsername" U.nil

testUpdateAccountEmail :: IO ()
testUpdateAccountEmail = updateAccountEmail "updatedEmail" U.nil

testUpdateAccountPassword :: IO ()
testUpdateAccountPassword = updateAccountPassword "updatedPassword" U.nil

testCreateAccount :: IO ()
testCreateAccount = createAccount (def :: Account)

testWithAccount :: (UUID -> IO ()) -> String -> IO ()
testWithAccount action accountIdStr =
  case U.fromString accountIdStr of
    Just accountId -> action accountId
    Nothing        -> action U.nil

testDeleteAccount :: IO ()
testDeleteAccount = deleteAccount U.nil
