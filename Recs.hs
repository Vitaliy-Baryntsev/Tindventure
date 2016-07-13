module Recs
  ( likeRecs
  , getRecs
  , swipeRight
  , swipeLeft
  , block
  ) where

import Control.Monad

import Text.JSON
import Network.Curl

import qualified Authorize as Auth


likeRecs :: IO ()
likeRecs = do
  recs <- getRecs
  --print recs
  mapM_ likeRec recs

getRecs ::  IO [Rec]
getRecs = do
  auth <- Auth.authorize
  (code,body) <- curlGetString "https://api.gotinder.com/user/recs"
    [ CurlHttpHeaders
      [ "X-Auth-Token: " ++ auth
      , "Content-Type: application/json"
      , "User-Agent: Tinder/3.0.4 (iPhone; iOS 7.1; Scale/2.00)" ] ]
  let recs = decode body
  putStrLn body
  case recs of
       Error e -> do
         putStrLn $ "error in getRecs: " ++ e
         return []
       Ok re -> return . results $ re

likeRec :: Rec -> IO ()
likeRec (Rec id) = do
  swipeRight id

swipeRight :: String -> IO ()
swipeRight id = do
  auth <- Auth.authorize
  (hdr,bdy) <- curlGetString ("https://api.gotinder.com/like/" ++ id)
    [ CurlHttpHeaders
      [ "X-Auth-Token: " ++ auth
      , "Content-Type: application/json"
      , "User-Agent: Tinder/3.0.4 (iPhone; iOS 7.1; Scale/2.00)" ] ]
  putStrLn bdy
  return ()

--does not unmatch, but probably works
swipeLeft :: String -> IO ()
swipeLeft id = do
  auth <- Auth.authorize
  (hdr,bdy) <- curlGetString ("https://api.gotinder.com/pass/" ++ id)
    [ CurlHttpHeaders
        [ "X-Auth-Token: " ++ auth
        , "Content-Type: application/json"
        , "User-Agent: Tinder/3.0.4 (iPhone; iOS 7.1; Scale/2.00)" ]
    , CurlPost True
    ]
  putStrLn bdy
  return ()

--DOES NOT WORK
block :: String -> String -> IO ()
block id cause = do
  auth <- Auth.authorize
  (hdr,bdy) <- curlGetString ("https://api.gotinder.com/report/" ++ id)
    [ CurlHttpHeaders
        [ "X-Auth-Token: " ++ auth
        , "Content-Type: application/json"
        , "User-Agent: Tinder/3.0.4 (iPhone; iOS 7.1; Scale/2.00)" ]
    , CurlPostFields [ "{\"cause\": " ++ cause ++ "}" ]
    , CurlPost True
    ]
  print hdr
  putStrLn bdy
  return ()

data Recs = Recs
  { results :: [Rec]
  } deriving(Show)

data Rec = Rec
  { _id :: String
  } deriving(Show)

instance JSON Recs where
  showJSON _ = JSNull
  readJSON (JSObject jso) =
    Recs `liftM`
      valFromObj "results" jso

instance JSON Rec where
  showJSON _ = JSNull
  readJSON (JSObject jso) =
    Rec `liftM`
      valFromObj "_id" jso
