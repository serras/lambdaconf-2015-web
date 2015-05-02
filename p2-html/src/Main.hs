{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Text
import Data.Text.Lazy (toStrict)
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A
import Text.Hamlet
import Web.Spock.Safe

main :: IO ()
main = runSpock 8080 $ spockT id $ do
  -- /hello/:name
  get ("hello" <//> var) $ \name ->
    html $ toStrict $ renderHtml $
      B.html $
        B.body $
          B.h1 $ do
            B.text "Hello "
            B.span ! A.style "color: red;" $ B.text name
            
  -- /allow/:age
  get ("allow" <//> var) $ \(age :: Integer) ->
    if age < 21
       then html $ toStrict $ renderHtml $
            [shamlet|<html>
                       <body>
                         <h1>
                           You are
                           <span style="color: red;">not
                           allowed
                         <p>You are only #{age} years old
            |]
       else html $ toStrict $ renderHtml $
            [shamlet|<html>
                       <body>
                         <h1>Please, come in
            |]
