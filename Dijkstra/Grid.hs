{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dijkstra.Grid (
  -- Grid coordinates
  Coord

  -- Basic grids
  , Grid(..)
  , gridFromList
  , (!!!), (!!?)
  , dims, subGrid, inBounds
  , minNeighbor, neighbors, potentialNeighbors

  -- Grids of (tropical) weights
  , Weighted
  , Weight

  -- Reachability (Bool) grids
  , Reachable
  , reachableToWeighted
  , reachable, reachables

  -- Grids of characters.
  , Chars
  , charsToReachable, charsToWeighted

  -- Printing and showing
  , showGrid, printGrid
  ) where

import TropicalSemiring

import Prelude hiding (concat, maximum, minimum)

import Control.Applicative (Applicative(..), pure, liftA2)
import Control.DeepSeq     (NFData(..))
import Control.Monad       (guard)
import Data.Foldable       (Foldable(..), concat, maximum, minimum)
import Data.Functor        ((<$>))
import Data.Maybe          (isJust)
import Data.Vector         (Vector, (!),)
import Data.Ix             (inRange)
import Linear.V2           (V2(..))

import Control.Lens        -- Yes, all of it.

import qualified Data.Vector as V

instance NFData a => NFData (V2 a) where rnf (V2 a b) = rnf a `seq` rnf b `seq` ()

type Coord = V2 Int

newtype Grid a = Grid { _cells :: Vector (Vector a) }
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance NFData a => NFData (Grid a) where
    rnf g = rnf (_cells g) `seq` ()

makeLenses ''Grid
makePrisms ''Grid

type instance Index (Grid a) = Coord
type instance IxValue (Grid a) = a

instance Applicative f => Ixed f (Grid a) where
    ix xy@(V2 x y) f (Grid xs) = Grid <$> ix y (ix x (indexed f xy)) xs

instance FunctorWithIndex Coord Grid
instance FoldableWithIndex Coord Grid
instance TraversableWithIndex Coord Grid where
    itraverse f (Grid xs) = Grid <$> itraverse (\y -> itraverse (\x -> f (V2 x y))) xs

-- (!!!) :: Grid a -> Coord -> a
grid !!! v = grid^?!ix v

-- (!!?) :: Grid a -> Coord -> Maybe a
grid !!? v = grid^?ix v

dims :: Grid a -> (Int, Int)
dims = liftA2 (,) (V.length . V.head) V.length . _cells

subGrid :: Int -> Coord -> [Coord]
subGrid width (V2 x y) =
    [ (V2 x' y')
    | y' <- [y-width..y+width]
    , x' <- [x-width..x+width]
    , (x,y) /= (x',y')
    , x' >= 0
    , y' >= 0
    ]

inBounds :: Grid a -> Coord -> Bool
inBounds g c = let (w,h) = dims g in inRange (V2 0 0, V2 w h - 1) c

gridFromList :: [[a]] -> Grid a
gridFromList = Grid . V.fromList . fmap V.fromList

minNeighbor :: Ord a => Grid a -> Coord -> Tropical a
minNeighbor g c = minimum $ Tropical . (g!!?) <$> potentialNeighbors c

neighbors :: Grid a -> Coord -> Vector Coord
neighbors g c = V.filter (\c -> isJust (g!!?c)) (potentialNeighbors c)

potentialNeighbors :: Coord -> Vector Coord
potentialNeighbors = V.fromList . subGrid 1

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

-- Helpers

printGrid :: Show a => Grid a -> IO ()
printGrid = putStrLn . showGrid

showGrid :: Show a => Grid a -> String
showGrid g = unlines . V.toList . fmap (concat . V.toList) $ padded where
  Grid padded = fmap (padL mx) shown
  shown = fmap show g
  mx = 1 + maximum (fmap length shown)

padL n s = let extra = n - (length s) in pad extra s where
  pad :: Int -> String -> String
  pad 0 s   = s
  pad 1 s   = ' ':s
  pad n s
    | n < 0 = s
    | n > 1 = ' ':pad (n-1) s

-- Optional Show instance
instance Show a => Show (Tropical a) where
    show (Tropical Nothing)  = "∞"
    show (Tropical (Just a)) = show a

