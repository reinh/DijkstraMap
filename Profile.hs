module Main where

import Linear.V2 (V2(..))

import Dijkstra.Map
import Dijkstra.Grid (printGrid)
import TestMaps

main = printGrid $ resolve (V2 0 0) testMap'''
