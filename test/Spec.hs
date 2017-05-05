{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import App (test)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return test) $ do
    describe "GET /" $ do
        it "responds with 200" $ do
            get "/" `shouldRespondWith` 200
    describe "GET /hello/gabi" $ do
        it "responds with greet" $ do
            let greeting = "{\"msg\":\"Hello, gabi\"}"
            get "/hello/gabi/" `shouldRespondWith` greeting
