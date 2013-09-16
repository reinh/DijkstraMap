module Main where

import Prelude hiding (all)

import qualified Data.Vector         as V

import Criterion.Main
import Linear.V2

import DijkstraMap
import Dijkstra.Grid
import TestMaps

main :: IO ()
main = defaultMain large

large = [ bench "resolve  large map" $ nf (resolve  $ V2 0 0) testMap'' ]

huge = [ bench "resolve  huge X1 map" $ nf (resolve  $ V2 20 20) testMap''' ]

all = [ bench "resolve  small map"   $ nf (resolve  $ V2 0 0) testMap
      , bench "resolve  medium map"  $ nf (resolve  $ V2 0 0) testMap'
      , bench "resolve  large map"   $ nf (resolve  $ V2 0 0) testMap''
      , bench "resolve  huge X1 map" $ nf (resolve  $ V2 20 20) testMap'''
      ]
