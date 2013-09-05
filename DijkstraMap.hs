{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}

module Main where

import Prelude hiding      (concatMap, sum, maximum, minimum, concat)

-- import Dijkstra.Grid       (Weighted, Coord, minNeighbor, (!!!))
import Dijkstra.Grid
import Dijkstra.BFS        (bfsOrder)
import TropicalSemiring
import TestMaps            (testMap, testMap')

import Data.List           (intersect, nub)
import Control.DeepSeq     (deepseq)
import Control.Monad       (join)
import Data.Functor        ((<$>))
import Control.Applicative ((<*>), pure)

import Control.Parallel.Strategies (using, parTraversable, rpar)

import Control.Lens

import qualified Data.Map            as M

resolve :: Coord -> Weighted -> Weighted
resolve c g | g' == g   = g
            | otherwise = resolve c g' where
              g' = resolveApproximate c g


resolveApproximate :: Coord -> Weighted -> Weighted
resolveApproximate c g = foldl go (target g c) (bfsOrder g c) where
  go :: Weighted -> Coord -> Weighted
  go g' c' = g' & ix c' %~ updateCell g' c'

updateCell :: Weighted -> Coord -> Tropical Weight -> Tropical Weight
updateCell g c v = update <$> v <*> m where
  update v m = min v (m + 1)
  m = join (minNeighbor g c)

target :: Weighted -> Coord -> Weighted
target g c = g & ix c .~ pure 0

asyncBuildMaps g = syncBuildMaps g `using` parTraversable rpar
syncBuildMaps g = ifoldr (\i _ m -> M.insert i (resolve i g) m) M.empty g

main :: IO ()
main = do
    let m = syncBuildMaps testMap
    m `deepseq` print "done"
