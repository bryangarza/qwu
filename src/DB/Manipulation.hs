{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.Manipulation (
    createPost
  , createAccount
  -- , deletePost
  -- , deleteAccount
  , updatePostBody
  -- , updateAccountUsername
  -- , updateAccountEmail
  -- , updateAccountPassword
  ) where

import DB.Connect
import qualified DB.Util as Util
import DB.Table.Post     (Post, Post'(Post), body, ts)
import DB.Table.Account  (Account, Account'(Account), username, email, password)
import qualified DB.Table.Post         as Post
import qualified DB.Table.Account      as Account
import qualified DB.Table.Relationship as Rel

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Clock as Clock
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
-- import qualified Data.Tuple.Select as TS
import qualified Data.UUID as U
import Database.PostgreSQL.Simple (Connection, close)
import Opaleye ((.==), runInsert, runDelete, runInsertReturning, runUpdate)
import Opaleye.PGTypes (pgStrictText, pgInt4, pgUUID, pgUTCTime)
import Opaleye.Column (Column)
import Opaleye.Internal.Table (Table)

-- someUpdate :: Util.MatchId (Table columnsW columnsR) idToMatch columnsR =>
--               Table columnsW columnsR
--            -> (columnsR -> columnsW) -> idToMatch -> IO ()
-- someUpdate table update idToMatch =
--   Util.runWithConn3 runUpdate table update match
--   where match = Util.matchId table idToMatch

-- deleteRow :: Util.MatchId (Table a columnsR) idToMatch columnsR =>
--              Table a columnsR -> idToMatch -> IO ()
-- deleteRow table = Util.runWithConn runDelete table . Util.matchId table

addPostToAccount :: Connection -> [Post.Id_] -> U.UUID -> IO ()
addPostToAccount conn [postId] accountId =
  do
    runInsert conn Rel.accountPostTable (pgInt4 postId, pgUUID accountId)
    close conn

createPost :: Post -> U.UUID -> IO ()
createPost Post {body} accountId =
  do
    -- ts     <- Clock.getCurrentTime
    conn   <- pConnect
    postId <- runInsertReturning conn Post.table columns select
    addPostToAccount conn postId accountId
  where
    body'                  = pgStrictText body
    Just ts               = Time.parseTime Time.defaultTimeLocale "%c" "Thu Jan  1 00:00:10 UTC 1970" :: Maybe Time.UTCTime
    ts'                    = pgUTCTime ts
    columns                = Post Nothing body' ts'
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

-- updateCardBack :: Card.Back -> Card.Id_ -> IO ()
-- updateCardBack newBack = updateCardField update
--   where
--     newBack' = pgStrictText newBack
--     update x = x { back = newBack' }

-- createDeck :: Deck -> U.UUID -> IO ()
-- createDeck Deck {title, desc} accountId =
--   do
--     conn   <- pConnect
--     deckId <- runInsertReturning conn Deck.table columns select
--     addDeckToAccount conn deckId accountId
--   where
--     title'                = pgStrictText title
--     desc'                 = pgStrictText desc
--     columns               = Deck Nothing title' desc'
--     select Deck {Deck.id_} = id_

-- updateDeckField :: (Deck.ColumnW -> Deck.ColumnW) -> Deck.Id_ -> IO ()
-- updateDeckField update idToMatch =
--   Util.runWithConn3 runUpdate Deck.table update' match
--   where
--     update' x@Deck {Deck.id_} = update (x { Deck.id_ = Just id_ })
--     match Deck {Deck.id_} = id_ .== pgInt4 idToMatch

-- updateDeckTitle :: Deck.Title -> Deck.Id_ -> IO ()
-- updateDeckTitle newTitle = updateDeckField update
--   where
--     newTitle' = pgStrictText newTitle
--     update x = x { title = newTitle' }

-- updateDeckDesc :: Deck.Desc -> Deck.Id_ -> IO ()
-- updateDeckDesc newDesc = updateDeckField update
--   where
--     newDesc' = pgStrictText newDesc
--     update x = x { desc = newDesc' }

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

-- updateAccountField :: (Account.ColumnR -> Account.ColumnW) -> Account.Id_ -> IO ()
-- updateAccountField update idToMatch =
--   Util.runWithConn3 runUpdate Account.table update match
--   where
--     match Account {Account.id_} = id_ .== pgUUID idToMatch

-- updateAccountUsername :: Account.Username -> Account.Id_ -> IO ()
-- updateAccountUsername newUsername = updateAccountField update
--   where newUsername' = pgStrictText newUsername
--         update x = x { username = newUsername' }

-- updateAccountEmail :: Account.Email -> Account.Id_ -> IO ()
-- updateAccountEmail newEmail = updateAccountField update
--   where
--     newEmail' = pgStrictText newEmail
--     update x = x { email = newEmail' }

-- updateAccountPassword :: Account.Password -> Account.Id_ -> IO ()
-- updateAccountPassword newPassword id_ =
--   do
--     hash <- Util.genPassword (encodeUtf8 newPassword)
--     updateAccountField (update hash) id_
--   where
--     update :: BS.ByteString -> Account.ColumnR -> Account.ColumnW
--     update hash x = x { password = hash' }
--       where hash' = pgStrictText (decodeUtf8 hash)

-- deleteCard :: Card.Id_ -> IO ()
-- deleteCard idToMatch = Util.runWithConn runDelete Card.table match
--   where
--     match Card {Card.id_} = id_ .== pgInt4 idToMatch

-- deleteDeck :: Deck.Id_ -> IO ()
-- deleteDeck idToMatch = Util.runWithConn runDelete Deck.table match
--   where
--     match Deck {Deck.id_} = id_ .== pgInt4 idToMatch

-- deleteAccount :: Account.Id_ -> IO ()
-- deleteAccount idToMatch = Util.runWithConn runDelete Account.table match
--   where
--     match Account {Account.id_} = id_ .== pgUUID idToMatch
