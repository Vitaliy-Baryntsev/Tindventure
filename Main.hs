module Main
  ( main
  ) where

import Control.Monad
import Data.Maybe

import Control.Concurrent (threadDelay)
import Network.Curl
import Text.JSON

import qualified Parse as P
import qualified Send
import qualified Authorize as Auth
import qualified Recs
import qualified Update as Up
import qualified Game


main = Auth.currentISO >>= processFrom

processFrom :: String -> IO ()
processFrom oldTime = do
  nextTime <- Auth.currentISO
  putStrLn $ "  " ++ oldTime
  --Recs.likeRecs
  putStrLn "skipping liking recs for now"
  replyUpdates oldTime
  threadDelay 10000000 --like 10 seconds or something
  processFrom nextTime

replyUpdates :: String -> IO ()
replyUpdates time = do
  update <- Up.fetchUpdate time
  -- update :: Result Update
  print update --Main place that updates should be printed
  let mes = P.getReceivedMessages `liftM` update
  case mes of
       Ok msgs -> mapM_ replyMessage msgs
       Error str -> putStrLn $ "couldn't fetch update: " ++ str

replyMessage :: P.Message -> IO ()
replyMessage msg = do
  resp <- Game.process msg
  Send.send (P.match_id msg) resp
  return ()

