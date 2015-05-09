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
      -- create user -- GET form
      get ("user" <//> "new") $ do
        html $ toStrict $ renderHtml $
          B.html $
            B.body $
              B.form ! A.action "/user1/new" ! A.method "POST" $ do
                B.label $ do "First name:"
                             B.input ! A.type_ "text" ! A.name "firstname"
                B.br
                B.label $ do "Last name:"
                             B.input ! A.type_ "text" ! A.name "lastname"
                B.br
                B.input ! A.type_ "submit" ! A.value "Register!"
      -- create user -- POST
      post ("user0" <//> "new") $ do
        (firstname_ :: Maybe String) <- param "firstname"
        (lastname_  :: Maybe String) <- param "lastname"
        case (firstname_, lastname_) of
          (Just firstname, Just lastname) -> text (pack firstname)
          (_, _) -> setStatus status500
      -- create user -- POST
      post ("user1" <//> "new") $ do
        firstname <- param "firstname"
        lastname  <- param "lastname"
        let user = User <$> notEmpty firstname <*> notEmpty lastname
        case user of
          Just u  -> do userId <- withDb $ insertUnique u
                        case userId of
                          Just uid -> text "Registration successful"
                          Nothing  -> text "That user already exists"
          Nothing -> text "Wrong user data"

      -- create user -- GET
      get ("user2" <//> "new") $ do
        v <- getForm "user-data" (userForm withDb)
        html $ toStrict $ renderHtml $
          B.html $ B.body $ userView v
      post ("user2" <//> "new") $ do
        (v, newU) <- runForm "user-data" (userForm withDb)
        case newU of
          Just u -> do withDb $ insertUnique u
                       text "Registration successful"
          Nothing -> html $ toStrict $ renderHtml $
                       B.html $ B.body $ userView v

notEmpty :: Maybe String -> Maybe String
notEmpty Nothing   = Nothing
notEmpty (Just "") = Nothing
notEmpty (Just x)  = Just x

-- Controller-like
userForm withDb =
  "u" .: checkM "User already exists"
                (\(User fn ln) -> isNothing <$> (withDb $ getBy $ UniqueName fn ln))
                (User <$> "firstname" .: check "Can't be empty" (not . null) (string Nothing)
                      <*> "lastname"  .: check "Can't be empty" (not . null) (string Nothing))

-- View-like
userView :: View B.Html -> B.Html
userView view = do
  form view "/user2/new" $ do
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
