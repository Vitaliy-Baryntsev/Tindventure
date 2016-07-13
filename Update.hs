module Update
  ( fetchUpdate
  ) where

import Text.JSON
import Network.Curl

import qualified Authorize as Auth
import qualified Parse as P


fetchUpdate :: String -> IO (Result P.Update)
fetchUpdate time = do
  upStr <- fetchUpdateString time
  let resUp = decode upStr
  --print resUp
  return resUp

fetchUpdateString :: String -> IO String
fetchUpdateString time = do
  auth <- Auth.authorize
  (code, body) <- curlGetString "https://api.gotinder.com/updates"
    [ CurlHttpHeaders [ "Content-Type: application/json"
                      , "User-Agent: Tinder/3.0.4 (iPhone; iOS 7.1; Scale/2.0)"
                      , "X-Auth-Token: " ++ auth ]
    , CurlPostFields ["{\"last_activity_date\": \""
                        ++ time ++ "\"}" ] ]
  return body

