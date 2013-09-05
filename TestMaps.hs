module TestMaps where

import Dijkstra.Grid

testMap :: Weighted
testMap = charsToWeighted grid where
  grid :: Chars
  grid = gridFromList
    [ "                 "
    , " #     # #     # "
    , " #     # #     # "
    , " ### ### ### ### "
    , "                 "
    , " ### ### ### ### "
    , " #     # #     # "
    , " #     # #     # "
    , "                 "
    ]

testMap' :: Weighted
testMap' = charsToWeighted grid where
  grid :: Chars
  grid = gridFromList
    [ "                         "
    , " #     # #     # #     # "
    , " #     # #     # #     # "
    , " ### ### ### ### ### ### "
    , "                         "
    , " ### ### ### ### ### ### "
    , " #     # #     # #     # "
    , " #     # #     # #     # "
    , "                         "
    , " #     # #     # #     # "
    , " #     # #     # #     # "
    , " ### ### ### ### ### ### "
    , "                         "
    , " ### ### ### ### ### ### "
    , " #     # #     # #     # "
    , " #     # #     # #     # "
    , "                         "
    ]

