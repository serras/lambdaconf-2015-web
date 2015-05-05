{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Monoid
import Database.Persist hiding (get)
import Database.Persist.Sql hiding (get)
import qualified Database.Persist as Db
import qualified Database.Persist.Sqlite as Sqlite
import Data.Text (pack)
import Network.HTTP.Types.Status
import Web.Spock.Safe

import Db

main :: IO ()
main = do
  -- Create the database
  Sqlite.runSqlite "example.db" $ Sqlite.runMigration migrateAll
  -- Initialize the connection pool
  runNoLoggingT $ Sqlite.withSqlitePool "example.db" 10 $ \pool ->
    NoLoggingT $ runSpock 8080 $ spockT id $ do
      -- small function for running db
      let withDb f = liftIO $ runSqlPersistMPool f pool
      -- create new user
      get ("user" <//> "new" <//> var <//> var) $ \fname lname -> do
        user <- withDb $ insertUnique (User fname lname)
        case user of
          Nothing -> text "Duplicate user"
          Just k  -> text ("New user with id " <> pack (show k))
      -- obtain user by id
      get ("user" <//> var) $ \userId -> do
        user <- withDb $ Db.get (UserKey $ SqlBackendKey userId)
        case user of
          Nothing -> setStatus status404
          Just u  -> json u
      -- obtain users with a certain username
      get ("user" <//> "by-name" <//> var) $ \name -> do
        users <- withDb $ selectList ([UserFirstName ==. name] ||. [UserLastName ==. name]) []
        json users
      -- obtain users with a certain username
      get ("user" <//> "by-name" <//> var <//> var <//> var) $ \name offset limit -> do
        users <- withDb $
          selectList ([UserFirstName ==. name] ||. [UserLastName ==. name])
                      [Asc UserFirstName, Asc UserLastName, OffsetBy offset, LimitTo limit]
        json users
