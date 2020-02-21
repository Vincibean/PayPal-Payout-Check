{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as B

main :: IO ()
main = hspec $ do
  describe "Aeson" $ do
      it "tinkerin with Aeson" $ do
        bs <- B.readFile "test-resources/colors.json"
        let exp = ["red","black"]
        let res = bs ^.. values . key "color" . _String
        res `shouldBe` exp
