{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Maybe (isNothing)
import Data.Monoid
import Database.Persist hiding (get)
import Database.Persist.Sql hiding (get)
import qualified Database.Persist as Db
import qualified Database.Persist.Sqlite as Sqlite
import Data.Text (pack)
import Data.Text.Lazy (toStrict)
import Network.HTTP.Types.Status
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A
import Text.Digestive hiding (text)
import Text.Digestive.Blaze.Html5
import Web.Spock.Digestive
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
      -- USER routes
      get  ("user" <//> "new") $ do
        v <- getForm "user-data" (userForm withDb)
        html $ toStrict $ renderHtml $
          B.html $ B.body $ userView v
      post ("user" <//> "new") $ do
        (v, newU) <- runForm "user-data" (userForm withDb)
        case newU of
          Just u  -> do withDb $ insertUnique u
                        text "Registration successful"
          Nothing -> html $ toStrict $ renderHtml $
                       B.html $ B.body $ userView v
      -- TASK routes
      get  ("task" <//> "new") $ do
        error "Implement me!"
      post ("task" <//> "new") $ do
        error "Implement me!"

      get  ("tasks") $ do
        error "Implement me!"
      post ("tasks") $ do
        error "Implement me!"

      -- JSON
      post ("json" <//> "user" <//> "new") $ do
        error "Implement me!"
      post ("json" <//> "task" <//> "new") $ do
        error "Implement me!"

-- USER
userForm withDb =
  "u" .: checkM "User already exists"
                (\(User fn ln) -> isNothing <$> (withDb $ getBy $ UniqueName fn ln))
                (User <$> "firstname" .: check "Can't be empty" (not . null) (string Nothing)
                      <*> "lastname"  .: check "Can't be empty" (not . null) (string Nothing))

userView :: View B.Html -> B.Html
userView view = do
  form view "/user/new" $ do
    label     "u.firstname" view "First name: "
    inputText "u.firstname" view
    errorList "u.firstname" view
    B.br
    label     "u.lastname" view "Last name: "
    inputText "u.lastname" view
    errorList "u.lastname" view
    B.br
    inputSubmit "Register!"
    errorList "u" view

-- TASK
taskForm withDb = error "Implement me!"

taskView :: View B.Html -> B.Html
taskView = error "Implement me!"
