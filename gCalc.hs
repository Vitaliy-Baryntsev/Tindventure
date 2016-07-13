module GCalc
  ( graphWith
  ) where

import Data.List

main = print $ graph (\x -> sin (exp x - 1) / x ) (-1,3)
--main = putStrLn $ graph (\x -> sin (exp x - 1) / x ) (-1,3)

graphWith :: Double -> Int ->
             (Double -> Double) ->
             (Double,Double) -> String
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

graph = graphWith 50 30
