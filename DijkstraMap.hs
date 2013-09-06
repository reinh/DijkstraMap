{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}

module Main where

import Prelude hiding      (concatMap, sum, maximum, minimum, concat)

-- import Dijkstra.Grid       (Weighted, Coord, minNeighbor, (!!!))
import Dijkstra.Grid
import Dijkstra.BFS
import TropicalSemiring
import TestMaps

import Data.List           (intersect, nub)
import Control.DeepSeq     (deepseq)
import Control.Monad       (join)
import Data.Functor        ((<$>))
import Data.Foldable       (foldl')
import Control.Applicative ((<*>), pure)

import Control.Parallel.Strategies (using, parTraversable, rpar)

import Control.Lens

import qualified Data.Map            as M
import qualified Data.Vector         as V

import Criterion.Main
import Linear.V2

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
  target = g & ix c .~ pure 0

  -- The breadth-first ordering to iterate over
  o :: V.Vector Coord
  o = bfsOrder g c

  -- Resolve as much of the map as possible in a single pass
  go :: Weighted -> Weighted
  go g = V.foldl' step g o where
    step g' c' = g' & ix c' %~ updateCell g' c'

  updateCell :: Weighted -> Coord -> Tropical Weight -> Tropical Weight
  updateCell g c v = update <$> v <*> join (minNeighbor g c) where
    update v m = min v (m + 1)

-- asyncBuildMaps g = syncBuildMaps g `using` parTraversable rpar
-- syncBuildMaps g = ifoldr (\i _ m -> M.insert i (resolve i g) m) M.empty g

main :: IO ()
main = defaultMain large

small = [ bench "resolve  large map" $ nf (resolve  $ V2 0 0) testMap'' ]

large = [ bench "resolve  small map"   $ nf (resolve  $ V2 0 0) testMap
        , bench "resolve  medium map"  $ nf (resolve  $ V2 0 0) testMap'
        , bench "resolve  large map"   $ nf (resolve  $ V2 0 0) testMap''
        , bench "resolve  huge X1 map" $ nf (resolve  $ V2 20 20) testMap'''
        ]

duplicate n g = Grid $ V.concatMap _cells $ V.replicate n g
