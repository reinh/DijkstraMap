{-# LANGUAGE ImplicitParams #-}

module Dijkstra.BFS (bfsOrder) where

import Dijkstra.Grid

import Linear.V2 (V2(..))
import Data.List (union)

-- Specs
import Test.Hspec

bfsOrder :: Grid a -> Coord -> [Coord]
bfsOrder g c@(V2 x y) = foldl go [] [1..maxDepth] where
  go :: [Coord] -> Int -> [Coord]
  go acc depth = acc `union` filter (inBounds g) (subGrid depth c)

  maxDepth :: Int
  maxDepth = maximum [x, y, (w-x), (h-y)] where (w,h) = dims g 

-- Specs

main :: IO ()
main = hspec $ do
  describe "bfsOrder" $ do
    it "traverses neighbors moving top-to-bottom, left-to-right" $ do
      bfsOrder smallGrid (V2 1 1) `shouldBe` [ V2 0 0, V2 1 0, V2 2 0
                                             , V2 0 1,         V2 2 1
                                             , V2 0 2, V2 1 2, V2 2 2 ]

    it "traverses neighbors starting from the source and expanding outwards" $ do
      bfsOrder mediumGrid (V2 2 2) `shouldBe` (inner ++ outer) where
        inner = [ V2 1 1, V2 2 1, V2 3 1
                , V2 1 2,         V2 3 2
                , V2 1 3, V2 2 3, V2 3 3 ]

        outer = [ V2 0 0, V2 1 0, V2 2 0, V2 3 0, V2 4 0
                , V2 0 1,                         V2 4 1
                , V2 0 2,                         V2 4 2
                , V2 0 3,                         V2 4 3
                , V2 0 4, V2 1 4, V2 2 4, V2 3 4, V2 4 4 ]


smallGrid :: Weighted
smallGrid = charsToWeighted grid where
  grid :: Chars
  grid = gridFromList
    [ "   "
    , "   "
    , "   "
    ]

mediumGrid :: Weighted
mediumGrid = charsToWeighted grid where
  grid :: Chars
  grid = gridFromList
    [ "     "
    , "     "
    , "     "
    , "     "
    , "     "
    ]
