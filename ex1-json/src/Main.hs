{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Aeson hiding (json)
import Web.Spock.Safe

data Person = Person { name    :: String
                     , age     :: Integer
                     , address :: String
                     }

listPeople :: [Person]
listPeople = [ Person "Fulanito"  30 "Boulder, CO"
             , Person "Menganito" 25 "Madrid, ES"
             , Person "Zutano"    35 "Utrecht, NL"
             ]

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  -- /hello/:name
  get ("hello" <//> var) $ \name ->
    json $ object [ "hello" .= String name ]
  -- /allow/:age
  get ("allow" <//> var) $ \(age :: Integer) ->
    error "Implement me!"
