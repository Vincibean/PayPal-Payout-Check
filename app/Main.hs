{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Test.WebDriver

main :: IO ()
main = runSession chromeConfig $ do
  openPage "http://google.com"
  searchInput <- findElem ( ByCSS "input[type='text']" )
  sendKeys "Hello, World!" searchInput
  submit searchInput
  closeSession


chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig