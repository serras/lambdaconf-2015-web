{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Monoid
import Web.Spock.Safe

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  -- /hello/:name
  get ("hello" <//> var) $ \name ->
    text ("Hello, " <> name)
  -- /allow/:age
  get ("allow" <//> var) $ \(age :: Integer) ->
    if age < 21
       then text "You are not allowed"
       else text "Please, come in"
  
  -- /hello-html/:name
  get ("hello-html" <//> var) $ \name ->
    html ("<html><body><h1>Hello, " <> name <> "</h1></body></html>")
