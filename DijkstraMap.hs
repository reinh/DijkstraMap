{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}

module Main where

import Prelude hiding      (concatMap, sum, maximum, minimum, concat)

-- import Dijkstra.Grid       (Weighted, Coord, minNeighbor, (!!!))
import Dijkstra.Grid
import Dijkstra.BFS        (bfsOrder)
import TropicalSemiring
import TestMaps            (testMap, testMap', testMap'')

import Data.List           (intersect, nub)
import Control.DeepSeq     (deepseq)
import Control.Monad       (join)
import Data.Functor        ((<$>))
import Control.Applicative ((<*>), pure)

import Control.Parallel.Strategies (using, parTraversable, rpar)

import Control.Lens

import qualified Data.Map            as M

import Criterion.Main
import Linear.V2

goFixed :: Eq a => (a -> a) -> a -> a
goFixed f g | g' == g  = g
            | otherwise = goFixed f g' where
              g' = f g

resolve :: Coord -> Weighted -> Weighted
resolve c = let f = resolveApproximate c in goFixed f

resolveApproximate :: Coord -> Weighted -> Weighted
resolveApproximate c g = foldl step (target g c) (bfsOrder g c) where
  step :: Weighted -> Coord -> Weighted
  step g' c' = g' & ix c' %~ updateCell g' c'

resolveSlow :: Coord -> Weighted -> Weighted
resolveSlow c g = goFixed (imap =<< updateCell) (target g c) where

updateCell :: Weighted -> Coord -> Tropical Weight -> Tropical Weight
updateCell g c v = update <$> v <*> m where
  update :: Num a => Ord a => a -> a -> a
  update v m = min v (m + 1)
  m = join (minNeighbor g c)

target :: Weighted -> Coord -> Weighted
target g c = g & ix c .~ pure 0

asyncBuildMaps g = syncBuildMaps g `using` parTraversable rpar
syncBuildMaps g = ifoldr (\i _ m -> M.insert i (resolve i g) m) M.empty g

main :: IO ()
main = defaultMain [
         bench "slow small map"  $ nf (resolveSlow (V2 5 5)) testMap
       , bench "fast small map"  $ nf (resolve     (V2 5 5)) testMap
       , bench "slow medium map" $ nf (resolveSlow (V2 5 5)) testMap'
       , bench "fast medium map" $ nf (resolve     (V2 5 5)) testMap'
       , bench "slow large map"  $ nf (resolveSlow (V2 5 5)) testMap''
       , bench "fast large map"  $ nf (resolve     (V2 5 5)) testMap''
       ]
