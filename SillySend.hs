module SillySend () where

import System.IO
import Control.Monad
import Data.List

import Text.JSON

import Send( send )
import People( getPerson )
import Update
import Parse
import Recs
import Authorize

import qualified People as P
import qualified Control.Monad.Parallel as Par


purgeDudes :: IO ()
purgeDudes = do
  upRes <- fetchUpdate ""
  case upRes of
       Error _ -> return ()
       Ok up -> mapM_ purgeDude (matches up)

purgeDude :: Match -> IO ()
purgeDude match = do
  let isMale = case isNew match of
        Nothing -> False
        Just person -> gender person == Male
  let isIgnored = null . messages $ match
  if isMale && isIgnored
     then --swipeLeft (_id match)
          --block (_id match) "1"
          print match
     else return ()

respondBigLetters :: String -> String -> IO ()
respondBigLetters match_id text = do
  let getLetter char =
        hGetContents =<< openFile ("SillyAlphabet/" ++ [char] ++ ".txt") ReadMode
      getLetter :: Char -> IO String
      sendWord wrd = send match_id =<< (liftM join . sequence . map getLetter $ wrd)
      sendWord :: String -> IO ()
  mapM_ sendWord (words text)

sendParagraphs :: String -> FilePath -> IO ()
sendParagraphs match_id file = do
  text <- readFile file
  mapM_ (send match_id) (lines text)

graphWith :: Double
          -> Int
          -> (Double -> Double)
          -> (Double,Double)
          -> String
graphWith samples width f (a,b) =
  let dx = (b-a) / samples
      xs = [a,a+dx..b]
      ys = map f xs
      maxY = maximum ys
      minY = minimum ys
      width' = fromIntegral width
      shift = map (\y -> (y-minY)*width'/(maxY-minY)) $ ys
      pLeft = map floor shift
      pRight = map (width-) pLeft
      comb l r = replicate l '-' ++ "0" ++ replicate r '-'
      lines = zipWith comb pLeft pRight
  in intercalate "\n" lines

graph :: (Double -> Double) -> (Double,Double) -> String
graph = graphWith 50 30


sendGoodGraphs :: String -> IO ()
sendGoodGraphs match_id =
  mapM_ (send match_id) [sinXoverX, xPowXRecip, xSinXSq, thiningSinXOverX]

sendPulse :: String -> IO ()
sendPulse match_id =
  send match_id $ xSinXSq ++ "\n" ++ (unlines . reverse . lines $ xSinXSq)

sinX = graph sin (0,12)
sinXoverX = graph (\x -> sin x / x) (-10,10)
xPowXRecip = graph (\x -> x**(1/x)) (0,4)
xSinXSq = graph (\x -> x * sin (x**2)) (0,4)
thiningSinXOverX = graph (\x -> sin (x**2) / x) (1,6)

pulseSquig = graphWith 100 30 (\x -> (1 - x**2) * sin (1 - x**2)) (-1,1)

