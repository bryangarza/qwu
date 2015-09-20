{-# LANGUAGE OverloadedStrings #-}
module DB.Test where

import Data.Default
import Data.Text
import Data.UUID as U

import qualified DB.Table.Post as P
import qualified DB.Table.Account as A
import qualified DB.Table.Relationship as R
import DB.Manipulation

testUpdatePostBody :: P.Id_ -> IO ()
testUpdatePostBody = updatePostBody "updated"

-- testUpdateCardBack :: C.Id_ -> IO ()
-- testUpdateCardBack = updateCardBack "updated"

-- testUpdateDeckTitle :: D.Id_ -> IO ()
-- testUpdateDeckTitle = updateDeckTitle "updated"

-- testUpdateDeckDesc :: D.Id_ -> IO ()
-- testUpdateDeckDesc = updateDeckDesc "updated"

testUpdateAccountUsername :: IO ()
testUpdateAccountUsername = updateAccountUsername "updatedUsername" U.nil

testUpdateAccountEmail :: IO ()
testUpdateAccountEmail = updateAccountEmail "updatedEmail" U.nil

testUpdateAccountPassword :: IO ()
testUpdateAccountPassword = updateAccountPassword "updatedPassword" U.nil

testCreatePost :: String -> IO ()
testCreatePost = testWithAccount $ createPost (def :: P.Post)

testCreateAccount :: IO ()
testCreateAccount = createAccount (def :: A.Account)

testWithAccount :: (UUID -> IO ()) -> String -> IO ()
testWithAccount action accountIdStr =
  case U.fromString accountIdStr of
    Just accountId -> action accountId
    Nothing        -> action U.nil

-- testDeleteAccount :: String -> IO ()
-- testDeleteAccount = testWithAccount deleteAccount
