module Main where

import Prelude hiding (all)

import qualified Data.Vector         as V

import Criterion.Main
import Linear.V2

import Dijkstra.Map
import Dijkstra.Grid
import Dijkstra.BFS
import TestMaps
import Control.Lens

main :: IO ()
main = defaultMain huge

huge = [ bench "resolve  huge X1 map" $ nf (resolve  $ V2 0 0) testMap''' ]

all = [ bench "resolve  small map"   $ nf (resolve  $ V2 0 0) testMap
      , bench "resolve  medium map"  $ nf (resolve  $ V2 0 0) testMap'
      , bench "resolve  large map"   $ nf (resolve  $ V2 0 0) testMap''
      , bench "resolve  huge X1 map" $ nf (resolve  $ V2 0 0) testMap'''
      ]
