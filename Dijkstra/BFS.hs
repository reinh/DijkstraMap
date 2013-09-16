{-# LANGUAGE ImplicitParams #-}

module Dijkstra.BFS where

import Dijkstra.Grid
import TropicalSemiring (Tropical(getTropical))

import Prelude hiding ((++))
import Data.Vector ((++))
import Linear.V2 (V2(..))
import Data.Maybe (isJust)

import qualified Data.Vector as V


-- An ordering of the squares of the grid that begins with the immediate
-- neighbors of the target cell and then expands breadth-first outward.
bfsOrder :: Weighted -> Coord -> V.Vector Coord
bfsOrder g c = V.filter reachable $ V.concatMap (around c) depths where
  reachable c = isJust $ g !!? c >>= getTropical
  depths = V.enumFromTo 1 (maxDepth g c)

-- The maximum depth to generate
maxDepth :: Grid a -> Coord -> Int
maxDepth g (V2 x y) = maximum [x, y, (w-x), (h-y)] where (w,h) = dims g 
