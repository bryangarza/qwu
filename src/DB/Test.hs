{-# LANGUAGE OverloadedStrings #-}
module DB.Test where

import Data.Default
import Data.Text
import Data.UUID as U

import qualified DB.Table.Post as P
import qualified DB.Table.Account as A
import qualified DB.Table.Relationship as R
import DB.Manipulation

testCreatePost :: IO ()
testCreatePost = createPost (def :: P.Post) U.nil

testUpdatePostBody :: P.Id_ -> IO ()
testUpdatePostBody = updatePostBody "updated"

testUpdateAccountUsername :: IO ()
testUpdateAccountUsername = updateAccountUsername "updatedUsername" U.nil

testUpdateAccountEmail :: IO ()
testUpdateAccountEmail = updateAccountEmail "updatedEmail" U.nil

testUpdateAccountPassword :: IO ()
testUpdateAccountPassword = updateAccountPassword "updatedPassword" U.nil

testCreateAccount :: IO ()
testCreateAccount = createAccount (def :: A.Account)

testWithAccount :: (UUID -> IO ()) -> String -> IO ()
testWithAccount action accountIdStr =
  case U.fromString accountIdStr of
    Just accountId -> action accountId
    Nothing        -> action U.nil

testDeleteAccount :: IO ()
testDeleteAccount = deleteAccount U.nil
