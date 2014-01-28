module Dijkstra.TestMaps where

import Dijkstra.Grid
import Dijkstra.BFS

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

testChars :: Chars
testChars = grid where
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


testReachable :: Reachable
testReachable = charsToReachable grid where
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

testDungeon :: Reachable
testDungeon = charsToReachable grid where
  grid :: Chars
  grid = gridFromList
    [ "#############################################################################"
    , "#    ####        ######       ####        ########       ####        ########"
    , "#    ####        ######       ####        ########       ####        ########"
    , "#### ####        ###### ##### ####        ######## ##### ####        ########"
    , "#### #### ############# ##### #### ############### ##### #### ###############"
    , "#### #### ############# ##### #### ############### ##### #### ###############"
    , "#### #### ############# ##### #### ############### ##### #### ###############"
    , "####      ############# #####      ############### #####      ###############"
    , "####                    #####                      #####                    #"
    , "####      ######        #####      ######        #######      ######        #"
    , "####      ######        #####      ######        #######      ######        #"
    , "################        #################        ###################        #"
    , "################        #################        ###################        #"
    , "################        #################        ###################        #"
    , "################        #################        ###################        #"
    , "################        #################        ###################        #"
    , "#################################################################### ########"
    , "#    ####        ######       ####        ########       ####        ########"
    , "#    ####        ######       ####        ########       ####        ########"
    , "#### ####        ###### ##### ####        ######## ##### ####        ########"
    , "#### #### ############# ##### #### ############### ##### #### ###############"
    , "#### #### ############# ##### #### ############### ##### #### ###############"
    , "#### #### ############# ##### #### ############### ##### #### ###############"
    , "####      ############# #####      ############### #####      ###############"
    , "####                    #####                    # #####                    #"
    , "####      ######        #####      ######        # #####      ######        #"
    , "####      ######        #####      ######        # #####      ######        #"
    , "################        #################        # #################        #"
    , "################        #################        # #################        #"
    , "################        #################        # #################        #"
    , "################        #################        # #################        #"
    , "################        #################          #################        #"
    , "#############################################################################"
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

testMap'' :: Weighted
testMap'' = charsToWeighted grid where
  grid :: Chars
  grid = gridFromList
    [ "                                         "
    , " #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### "
    , "                                         "
    , " ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # "
    , "                                         "
    , " #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### "
    , "                                         "
    , " ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # "
    , "                                         "
    , " #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### "
    , "                                         "
    , " ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # "
    , "                                         "
    ]


testMap''' :: Weighted
testMap''' = charsToWeighted grid where
  grid :: Chars
  grid = gridFromList
    [ "                                                                         "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                         "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                         "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                         "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                         "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                         "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                         "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                         "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                         "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                         "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                         "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                         "
    ]


testMap'''' :: Weighted
testMap'''' = charsToWeighted grid where
  grid :: Chars
  grid = gridFromList
    [ "                                                                                                                                                 "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , "                                                                                                                                                 "
    , " ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , " #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # #     # "
    , "                                                                                                                                                 "
    ]
