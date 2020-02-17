{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.State
import Test.WebDriver
import Test.WebDriver.Session

main :: IO ()
main = runSession chromeConfig $ do
  login
  wait $ 5 * seconds
  logout
  wait $ 2 * seconds
  closeSession

login :: WD ()
login = do
  openPage "https://www.paypal.com/login"
  usr <- findElem ( ById "email" )
  sendKeys "usr" usr
  wait $ 1 * seconds
  btnNext <- findElem ( ById "btnNext" )
  click btnNext
  wait $ 3 * seconds
  pwd <- findElem ( ById "password" )
  sendKeys "pwd" pwd
  btnLogin <- findElem ( ById "btnLogin" )
  wait $ 1 * second
  click btnLogin

logout :: WD ()
logout = do
  btnLogout <- findElem ( ByClass "css-1fo6ps" )
  click btnLogout

second :: Int
second = 1000000

seconds :: Int
seconds = second

chromeConfig :: WDConfig
chromeConfig = useBrowser chrome defaultConfig

wait :: Int -> WD ()
wait x = io $ threadDelay x

io :: IO a -> WD a
io = WD . liftIO