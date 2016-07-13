module Database
  ( get
  , set
  ) where

import qualified Game.State as G

get :: String -> IO G.GameState
get _ = return G.State1

set :: String -> G.GameState -> IO ()
set _ _ = return ()
