module Main where

import Dijkstra.BFS
import TestMaps
import Linear.V2
import Control.Monad
import qualified Data.Vector as V

-- import Criterion.Main

main = testAround

testAround' = [1..1000] `forM_` \_ ->
  print $ around' (V2 0 0) 40

testAround = [1..1000] `forM_` \_ ->
  print $ around (V2 0 0) 40
    
-- testBfsOrder = let map = testMap''' in
--   print $ bfsOrder map (V2 0 0)

-- [ bench "V2 0 0" $ nf (bfsOrder testMap''') (V2 0 0)
-- , bench "V2 40 40" $ nf (bfsOrder testMap''') (V2 40 40)
-- ]
