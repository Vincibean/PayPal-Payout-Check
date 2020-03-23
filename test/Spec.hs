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
        (claimed', unclaimed') <- runSession chromeConfig $ do
          openPage $ "file://" ++ wd ++  "/test-resources/data.html"
          el <- findElem (ByXPath "//table/tbody/tr[1]/td[3]")
          claimed <- getText el
          nel1 <- findElem (ByXPath "//table/tbody/tr[2]/td[3]")
          unclaimed1 <- getText nel1
          nel2 <- findElem (ByXPath "//table/tbody/tr[3]/td[3]")
          unclaimed2 <- getText nel2
          nel3 <- findElem (ByXPath "//table/tbody/tr[4]/td[3]")
          unclaimed3 <- getText nel3
          nel4 <- findElem (ByXPath "//table/tbody/tr[5]/td[3]")
          unclaimed4 <- getText nel4
          nel5 <- findElem (ByXPath "//table/tbody/tr[6]/td[3]")
          unclaimed5 <- getText nel5
          let unclaimed = [unclaimed1, unclaimed2, unclaimed3, unclaimed4, unclaimed5]
          return (claimed, unclaimed)
        claimed' `shouldBe` "£8.90 GBP"
        unclaimed' `shouldBe` ["£0.01 GBP", "£0.02 GBP", "£0.03 GBP", "£0.04 GBP", "£0.05 GBP"]

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig