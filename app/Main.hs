{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad.State
import Control.Lens
import Data.Aeson.Lens
import Data.Text 
import Data.Text.Encoding (encodeUtf8)
import System.Random
import Test.WebDriver
import Test.WebDriver.Session

type TransactionId = Text
type Total = Double
type Unclaimed = Total
type TransactionReport = (Total, Unclaimed)

main :: IO ()
main = runSession chromeConfig $ do
  login
  waitIn <- io $ randomRIO (1 * second, 10 * seconds)
  wait $ waitIn * seconds
  
  transactionIds <- getTransactionIds
  fullTransactionReport <- traverse getTransactionReport transactionIds


  logout
  waitOut <- io $ randomRIO (1 * second, 5 * seconds)
  wait $ waitOut
  closeSession

login :: WD ()
login = do
  openPage "https://www.paypal.com/login"
  usr <- findElem ( ById "email" )
  sendKeys "usr" usr
  waitForUsr <- io $ randomRIO (1 * second, 5 * seconds)
  wait $ waitForUsr
  btnNext <- findElem ( ById "btnNext" )
  click btnNext
  waitForPwd <- io $ randomRIO (1 * second, 5 * seconds)
  wait $ waitForPwd
  pwd <- findElem ( ById "password" )
  sendKeys "pwd" pwd
  btnLogin <- findElem ( ById "btnLogin" )
  waitToLogin <- io $ randomRIO (1 * second, 5 * seconds)
  wait $ waitToLogin
  click btnLogin

getTransactionIds :: WD [TransactionId]
getTransactionIds = do 
  openPage "https://www.paypal.com/listing/transactions/activity?transactiontype=PAYOUTS&currency=ALL_TRANSACTIONS_CURRENCY&limit=&next_page_token=&need_actions=true&need_shipping_info=true&sort=time_created&archive=ACTIVE_TRANSACTIONS&entrypoint=&fromdate_year=2020&fromdate_month=1&fromdate_day=17&todate_year=2020&todate_month=1&todate_day=17"
  src <- getSource
  return $ extractTransactionIds src

extractTransactionIds :: Text -> [TransactionId]
extractTransactionIds txt = bs ^.. key "data".key "transactions".values.key "transactionId"._String
 where bs = encodeUtf8 txt

getTransactionReport :: TransactionId -> WD TransactionReport
getTransactionReport transactionId = do
  let tId = unpack transactionId
  openPage $ "https://www.paypal.com/activity/masspay/" ++ tId
  undefined

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