{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Text (pack)
import Web.Spock.Safe

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  lstVar <- liftIO $ newTVarIO ([] :: [String])
  -- /show
  get "show" $ do
    lst <- liftIO $ atomically $ readTVar lstVar
    text $ pack (show lst)
  -- /add/:name
  get ("add" <//> var) $ \(name :: String) -> do
    liftIO $ atomically $ modifyTVar lstVar (++ [name])
    text "OK"
  -- /remove/:n
  get ("remove" <//> var) $ \(n :: Int) -> do
    liftIO $ atomically $ modifyTVar lstVar $ \lst ->
      take n lst ++ drop (n + 1) lst
    text "OK"
