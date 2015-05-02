{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Web.Spock.Safe

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  -- /show
  get "show" $ do
    return ()
  -- /add/:name
  get ("add" <//> var) $ \(name :: String) -> do
    return ()
  -- /remove/:n
  get ("remove" <//> var) $ \(n :: Int) -> do
    return ()
