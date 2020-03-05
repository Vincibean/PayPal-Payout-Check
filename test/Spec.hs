{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Control.Lens
import Data.Aeson.Lens
import qualified Data.ByteString as B
import Test.WebDriver
import Test.WebDriver.Session

import System.Directory (getCurrentDirectory)

main :: IO ()
main = hspec $ do
  describe "Aeson" $ do
      it "tinkering with Aeson (colors.json)" $ do
        bs <- B.readFile "test-resources/colors.json"
        let exp = ["red","black"]
        let res = bs^..values.key "color"._String
        res `shouldBe` exp
      it "tinkering with Aeson (data.json)" $ do
        bs <- B.readFile "test-resources/data.json"
        let exp = ["MPA-8XFV5Q9HR5STQ","MPA-FG3CJBY74AY84","MPA-2BC62X5BCSPL6","MPA-NCMYMPFNJG4RU","MPA-ELPBVE29KFNRL","MPA-9T33RW8E75756","MPA-76SNKUN48ZBTA","MPA-NS9ZT8SBN8MRE","MPA-EJMHER2PWZELW","MPA-ZKJR92DLGR3NU","MPA-UFY3JZ9CF4UUJ","MPA-UK6ZZWHMGNN6L"]
        let res = bs ^.. key "data".key "transactions".values.key "transactionId"._String
        res `shouldBe` exp
  describe "webdriver" $ do
      it "tinkering with XPath (data.html)" $ do
        wd <- getCurrentDirectory
        txt' <- runSession chromeConfig $ do
          openPage $ "file://" ++ wd ++  "/test-resources/data.html"
          el <- findElem (ByXPath "//table/tbody/tr[1]/td[3]")
          txt <- getText el
          return txt
        txt' `shouldBe` "£8.90 GBP"

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig