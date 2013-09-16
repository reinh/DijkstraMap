{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}

module DijkstraMap where

import Prelude hiding      (concatMap, sum, maximum, minimum, concat)

-- import Dijkstra.Grid       (Weighted, Coord, minNeighbor, (!!!))
import Dijkstra.FlatGrid
import TropicalSemiring
import TestMaps

import Data.List           (intersect, nub)
import Control.DeepSeq     (deepseq)
import Control.Monad       (join)
import Data.Functor        ((<$>))
import Data.Foldable       (foldl')
import Control.Applicative ((<*>), pure)

import Control.Parallel.Strategies (using, parTraversable, rpar, rdeepseq)

import Control.Lens

import qualified Data.Vector         as V

goFixed :: Eq a => (a -> a) -> a -> a
goFixed f g | g' == g  = g
            | otherwise = goFixed f g' where
              g' = f g

-- Start by resolving the map twice (which is sufficient for many cases),
-- then continue searching for a fixed point by repeatedly resolving the map.
resolve :: Coord -> Weighted -> Weighted
resolve c g = goFixed go . (go.go) $ target where
  -- The grid with the target coordinate zeroed out
  target :: Weighted
  target = g & cells.ix (coordToIndex g c) .~ pure 0

  -- The breadth-first ordering to iterate over
  o :: V.Vector Coord
  o = bfsOrder g c

  -- Resolve as much of the map as possible in a single pass
  go :: Weighted -> Weighted
  go g = V.foldl' step g o where
    step g' c' = g' & cells.ix (coordToIndex g' c') %~ updateCell g' c'

  updateCell :: Weighted -> Coord -> Tropical Weight -> Tropical Weight
  updateCell g c v = update <$> v <*> join (minNeighbor g c) where
    update v m = min v (m + 1)

syncBuildGrids :: Weighted -> Grid Weighted
syncBuildGrids g = imap (\i _ -> resolve i g) g

asyncBuildGrids g = syncBuildGrids g `using` parTraversable rpar
