module Authorize
  ( authorize
  , currentISO
  , writeAuthorize
  ) where

import Control.Monad

import Network.Curl
import Data.Time.Clock
import Data.Time.ISO8601


currentISO :: IO String
currentISO = liftM formatISO8601Javascript getCurrentTime

authTokenFilePath :: String
authTokenFilePath = "AuthToken.txt"

authorize :: IO String
authorize = readFile authTokenFilePath

writeAuthorize :: IO String
writeAuthorize = fullAuthorize >== writeFile authTokenFilePath

fullAuthorize :: IO String
fullAuthorize = do
  tok <- fbToken
  case tok of
    Just tok ->
      xAuthToken tok
    Nothing -> do
      return "8bb6fd10-7d52-4110-be8c-4c3bf6936a90"
      --Default token that once worked

fbToken :: IO (Maybe String)
fbToken = do
  (CurlResponse a b c hdrs bdy d) <- curlGetResponse_ facebookURL
    [ CurlHttpHeaders
        [ "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:34.0) Gecko/20100101 Firefox/34.0"
        , "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
        , "Cookie: " ++ cookieData ]
    , CurlHttpGet True ]
        :: IO CurlResponse
  let mloc = lookup "Location" hdrs
      --Loc url is of form
      --   ..._token=CA...ZD&expires_...
      tok = (tail . takeWhile (/= '&') . dropWhile (/= '=') ) `liftM` mloc
  return tok

xAuthToken :: String -> IO String
xAuthToken fbAuth = do
  (code,body) <- curlGetString "https://api.gotinder.com/auth"
    [ CurlPost True
    , CurlNoBody False
    , CurlHttpHeaders [ "Content-Type: application/json"
                      , "User-Agent: Tinder/3.0.4 (iPhone; iOS 7.1; Scale/2.00)" ]
    , CurlPostFields [ "{\"facebook_token\": \"" ++ fbAuth
                  ++ "\",\"facebook_id\": \"" ++ facebookId ++ "\"}" ] ]
  return . takeWhile (/= '"') . drop 2 . dropWhile (/= ':') $ body
