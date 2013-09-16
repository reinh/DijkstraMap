{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Dijkstra.FlatGrid where

import Prelude hiding (concat, replicate, maximum, minimum)

import Linear.V2           (V2(..))
import Control.DeepSeq     (NFData(..))
import Control.Lens        -- Yes, all of it.
import Control.Applicative (Applicative(..), pure, liftA2)
import Data.Ix             (inRange)
import Data.Functor
import Data.Foldable
import Data.Tuple
import Data.List.Split     (chunksOf)
import Data.Maybe          (isJust)

import Data.Default
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Deriving

import Data.Vector         (Vector, (!),)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G

import TropicalSemiring
import Dijkstra.Grid (around)

derivingUnbox "Tropical"
  [t| (Default a, U.Unbox a) => Tropical a -> (Bool, a) |]
  [|  \(Tropical t) -> maybe (False, def) (\x -> (True, x)) t |]
  [| \(b,x) -> if b then Tropical (Just x) else Tropical Nothing |]

-- instance NFData a => NFData (V2 a) where rnf (V2 a b) = rnf a `seq` rnf b `seq` ()

instance NFData a => NFData (Grid a) where
    rnf g = rnf (_cells g) `seq` ()

type Coord = V2 Int

data Grid a = Grid { _w :: !Int, _h :: !Int, _cells :: Vector a }
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

makeLenses ''Grid
makePrisms ''Grid

inBounds :: Grid a -> Coord -> Bool

inBounds (Grid w h cells) = inRange (V2 0 0, V2 w h - 1)

coordToIndex :: Grid a -> Coord -> Int
coordToIndex (Grid w _ _) (V2 x y) = y*w+x

indexToCoord :: Grid a -> Int -> Coord
indexToCoord (Grid w _ _) i = uncurry V2 . swap $ divMod i w

g !!? c
  | inBounds g c = Just $ unsafeIx g c
  | otherwise    = Nothing

g !!! c
  | inBounds g c = unsafeIx g c
  | otherwise    = error "out of bounds error"

unsafeIx g c = g^?! cells.ix (coordToIndex g c)

instance FunctorWithIndex Coord Grid
instance FoldableWithIndex Coord Grid
instance TraversableWithIndex Coord Grid where
    itraverse f g@(Grid w h xs) = Grid w h <$> itraverse (f . indexToCoord g) xs where

-- An ordering of the squares of the grid that begins with the immediate
-- neighbors of the target cell and then expands breadth-first outward.
bfsOrder :: Grid (Tropical Int) -> Coord -> V.Vector Coord
bfsOrder g c = V.filter reachable $ V.concatMap (around c) depths where
  reachable c = isJust $ g !!? c >>= getTropical
  depths = V.enumFromTo 1 (maxDepth g c)

-- The maximum depth to generate
maxDepth :: Grid a -> Coord -> Int
maxDepth (Grid w h _) (V2 x y) = maximum [x, y, (w-x), (h-y)]

minNeighbor :: Ord a => Grid a -> Coord -> Tropical a
minNeighbor g c = minimum $ Tropical . (g!!?) <$> potentialNeighbors c

neighbors :: Grid a -> Coord -> Vector Coord
neighbors g c = V.filter (\c -> isJust (g!!?c)) (potentialNeighbors c)

potentialNeighbors :: Coord -> Vector Coord
potentialNeighbors c = V.map (+c) $ V.fromList
  [ V2 (-1) (-1), V2 0 (-1), V2 1 (-1)
  , V2 (-1)   0 ,            V2 1   0
  , V2 (-1)   1 , V2 0   1 , V2 1   1  ]

-- Cell Grids

data Cell = Floor | Wall deriving (Show, Read, Eq)
type Cells = Grid Cell

-- Reachability Grids

type Reachable = Grid Bool
reachable :: Cell -> Bool
reachable = (==Floor)

reachables :: Grid Cell -> Grid Bool
reachables = fmap reachable

-- Weight Grids

type Weight = Int
type Weighted = Grid (Tropical Weight)

reachableToWeighted :: Reachable -> Weighted
reachableToWeighted = fmap f where
  f True = pure (2^32) -- A large number, but smaller than infinity
  f False = infinity

-- Character Grids

type Chars = Grid Char

charsToReachable :: Chars -> Reachable
charsToReachable = fmap f where
  f ' ' = True
  f '#' = False

charsToWeighted :: Chars -> Weighted
charsToWeighted = reachableToWeighted . charsToReachable


replicate w h x = Grid w h $ V.replicate (w*h) x

gridFromList :: [[a]] -> Grid a
gridFromList []   = Grid 0 0 V.empty
gridFromList [[]] = Grid 0 0 V.empty
gridFromList xs   = Grid w h $ V.fromList (concat xs) where
  w = (length.head) xs
  h = length xs

showGrid :: Show a => Grid a -> String
showGrid (Grid w h cells) = unlines . fmap concat . chunksOf w . V.toList $ V.map padded cells where
  padded cell = padL mx (show cell)
  mx = 1 + (maximum $ V.map (length.show) cells)

printGrid :: Show a => Grid a -> IO ()
printGrid = putStrLn . showGrid

padL n s = let extra = n - (length s) in pad extra s where
  pad :: Int -> String -> String
  pad 0 s   = s
  pad 1 s   = ' ':s
  pad n s
    | n < 0 = s
    | n > 1 = ' ':pad (n-1) s


