module Send
  ( send
  ) where

import Network.Curl

import qualified Authorize as Auth

send :: String -> String -> IO ()
send match_id text = do
  auth <- Auth.authorize
  --putStrLn $ text ++ " to " ++ match_id
  (code,body) <- curlGetString ("https://api.gotinder.com/user/matches/" ++ match_id)
    [ CurlPost True
    , CurlNoBody False
    , CurlHttpHeaders [ "X-Auth-Token: " ++ auth
                      , "Content-Type: application/json"
                      , "User-Agent: Tinder/3.0.4 (iPhone; iOS 7.1; Scale/2.0)" ]
    , CurlPostFields [ "{\"message\": " ++ show text ++ "}" ] ]
  putStrLn body
  return ()

