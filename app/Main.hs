{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.State
import Test.WebDriver
import Test.WebDriver.Session

main :: IO ()
main = runSession chromeConfig $ do
  login
  wait 5000000
  logout
  wait 2000000
  closeSession

login :: WD ()
login = do
  openPage "https://www.paypal.com/login"
  usr <- findElem ( ById "email" )
  sendKeys "usr" usr
  wait 1000000
  btnNext <- findElem ( ById "btnNext" )
  click btnNext
  wait 2500000
  pwd <- findElem ( ById "password" )
  sendKeys "pwd" pwd
  btnLogin <- findElem ( ById "btnLogin" )
  wait 1000000
  click btnLogin

logout :: WD ()
logout = do
  btnLogout <- findElem ( ByClass "css-1fo6ps" )
  click btnLogout


chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

wait :: Int -> WD ()
wait x = io $ threadDelay x

io :: IO a -> WD a
io = WD . liftIO