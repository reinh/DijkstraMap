module Main where

import Linear.V2 (V2(..))

import Dijkstra.Map (resolve)
import Dijkstra.Grid (printGrid)
import Dijkstra.TestMaps

main = printGrid $ resolve (V2 0 0) testMap'''
