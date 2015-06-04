{-# LANGUAGE OverloadedStrings #-}

module AuthSpec where

import Data.Time.Format
import Test.Hspec

import Moz.Auth

spec :: Spec
spec = do
  describe "signature" $ do
    it "generates a Base64 API signature for the given Auth and time" $ do
      let time = parseTimeOrError False defaultTimeLocale "%Y-%m-%d" "2015-01-01"
          auth = Auth "member-123" "foobar123"
      signature auth time `shouldBe` "guUxf8wqxtQMbIeYPNOUxtVcXnY="
