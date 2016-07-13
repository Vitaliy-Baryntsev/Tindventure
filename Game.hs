module Game
  ( process
  ) where

import qualified Parse as P
import qualified Database as DB

import Data.Char

process :: P.Message -> IO String
process msg = do
  putStrLn "supposedly processing in game"
  return (fmap toUpper $ P.message msg)
