{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.State
import System.Random
import Test.WebDriver
import Test.WebDriver.Session

main :: IO ()
main = runSession chromeConfig $ do
  login
  waitIn <- io $ randomRIO (1, 10)
  wait $ waitIn * seconds
  logout
  waitOut <- io $ randomRIO (1, 5)
  wait $ waitOut * seconds
  closeSession

login :: WD ()
login = do
  openPage "https://www.paypal.com/login"
  usr <- findElem ( ById "email" )
  sendKeys "usr" usr
  waitForUsr <- io $ randomRIO (1, 5)
  wait $ waitForUsr * seconds
  btnNext <- findElem ( ById "btnNext" )
  click btnNext
  waitForPwd <- io $ randomRIO (1, 5)
  wait $ waitForPwd * seconds
  pwd <- findElem ( ById "password" )
  sendKeys "pwd" pwd
  btnLogin <- findElem ( ById "btnLogin" )
  waitToLogin <- io $ randomRIO (1, 5)
  wait $ waitToLogin * seconds
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