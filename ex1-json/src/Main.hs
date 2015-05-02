{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Aeson hiding (json)
import Web.Spock.Safe

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  -- /hello/:name
  get ("hello" <//> var) $ \name ->
    json $ object [ "hello" .= String name ]
  -- /allow/:age
  get ("allow" <//> var) $ \(age :: Integer) ->
    error "Implement me!"
