{-# LANGUAGE BangPatterns #-}

module Dijkstra.BFS where

import Dijkstra.Grid
import Dijkstra.Tropical (Tropical(getTropical))

import Prelude hiding ((++))
import Data.Vector ((++))
import Linear.V2 (V2(..), _x, _y, perp)
import Data.Maybe (isJust)
import Control.Lens

import qualified Data.Vector as V

-- An ordering of the squares of the grid that begins with the immediate
-- neighbors of the target cell and then expands breadth-first outward.
bfsOrder :: Weighted -> Coord -> V.Vector Coord
bfsOrder g c = V.filter reachable $ V.concatMap (around c) depths where
  reachable c = isJust $ g !!? c >>= getTropical
  depths = V.enumFromTo 1 20
  -- depths = V.enumFromTo 1 (maxDepth g c)

-- The maximum depth to generate
maxDepth :: Grid a -> Coord -> Int
maxDepth g (V2 x y) = maximum [x, y, (w-x), (h-y)] where (w,h) = dims g 

-- The coordinates in a box around the target coordinate starting n cells
-- away
around :: Coord -> Int -> V.Vector Coord
around v2 n = top V.++ right V.++ bottom V.++ left where
  enum = V.enumFromTo
  negn = (-n)
  a = enum  (-n)  n
  b = enum (1-n) (n-1)
  top    = do { x <- a ; return $ v2 + V2 x  negn }
  right  = do { y <- b ; return $ v2 + V2 n    y  }
  bottom = do { x <- a ; return $ v2 + V2 x    n  }
  left   = do { y <- b ; return $ v2 + V2 negn y  }

-- The coordinates in a box around the target coordinate starting n cells
-- away
around' :: Coord -> Int -> V.Vector Coord
around' v2 n = top V.++ right V.++ bottom V.++ left where
  enum = V.enumFromTo
  top    = do { x <- enum  (-n)  n    ; return $ v2 + V2 x  (-n) }
  right  = do { y <- enum (1-n) (n-1) ; return $ v2 + V2 n    y  }
  bottom = do { x <- enum  (-n)  n    ; return $ v2 + V2 x    n  }
  left   = do { y <- enum (1-n) (n-1) ; return $ v2 + V2 (-n) y  }

-- XXX: Why the hell is this slower in resolve?
around'' :: Coord -> Int -> V.Vector Coord
around'' v2 n = V.map (+v2) . V.concat . take 4 . iterate (V.map perp) $ do
    x <- V.enumFromTo (1-n) n
    return $ V2 x n

display :: V.Vector Coord -> IO ()
display cs = printGrid gridded where
  xs = cs^..traverse._x
  ys = cs^..traverse._y
  grid = Grid $ V.replicate (1 + maximum ys - minimum ys) $
    V.replicate (1 + maximum xs - minimum xs) ""
  gridded = V.foldl' go grid cs where
    go g c = g & ix (c - V.minimum cs) .~ (show c)
