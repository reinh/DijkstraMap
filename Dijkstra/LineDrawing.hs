module Dijkstra.LineDrawing
  ( line
  , main
  , showLine
  , printLine
  )
  where

import Test.Hspec

import Data.Functor((<$>))
import Data.Array(listArray, (//))
import Data.Ratio((%))

import Linear.V2(V2(..))
import Linear.Vector(lerp)

import Dijkstra.Grid

-- | A scan-conversion algorithm for drawing lines on integral coordinate
-- systems. Returns the list of coordinates that should be filled to draw
-- the line from point to point.
line ::  (Integral a) => V2 a -> V2 a -> [V2 a]
line p p' = fmap near [0..end] where
  end    = case abs (p - p') of V2 dx dy -> max dx dy
  near i = round <$> lerp (i % end)
                          (fmap fromIntegral p')
                          (fmap fromIntegral p)

main ::  IO ()
main = hspec $ do
  describe "line" $ do
    it "returns the coordinates for the line" $ do
      line (V2 0 0) (V2 2  0) `shouldBe` ([V2 0 0, V2 1 0 , V2 2 0] :: [V2 Int])
      line (V2 5 8) (V2 9 11) `shouldBe` ([V2 5 8, V2 6 9, V2 7 10, V2 8 10, V2 9 11] :: [V2 Int])

data Value = Fill | Empty

instance Show Value where
  show Fill  = "◼"
  show Empty = "◻"

showLine ::  [V2 Int] -> String
showLine line = showGrid . Grid $ arr // (zip line (repeat Fill)) where
  arr = listArray (V2 0 0, maximum line) (repeat Empty)

printLine = putStrLn . showLine
