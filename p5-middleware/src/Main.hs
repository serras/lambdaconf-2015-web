{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Network.Wai.Middleware.Static
import Web.Spock.Safe

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  middleware $ staticPolicy (addBase "static")
  -- /hello/:name
  get ("hello" <//> var) $ \name ->
    text ("Hello, " <> name)
