{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dijkstra.Grid.Vector (
  -- Basic grids
    Grid(..)
  , gridFromList
  , (!!!), (!!?)
  , dims, subGrid, inBounds
  , minNeighbor, neighbors, potentialNeighbors
  , minNeighbor'

  -- Printing and showing
  , showGrid, printGrid
  ) where

import Prelude hiding (concat, maximum, minimum)

import Control.Applicative (Applicative(..), pure, liftA2)
import Control.DeepSeq     (NFData(..))
import Data.Foldable       (Foldable(..), concat, maximum, minimum)
import Data.Functor        ((<$>))
import Data.Maybe          (isJust)
import Data.Vector         (Vector, (!), (!?))
import Data.Ix             (inRange)
import Linear.V2           (V2(..))

import Control.Lens        -- Yes, all of it.

import qualified Data.Vector as V

import Dijkstra.Tropical
import Dijkstra.Coord


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

(Grid cs) !!! (V2 x y) = cs ! y ! x
{-# INLINE (!!!) #-}

(Grid cs) !!? (V2 x y) = cs !? y >>= (!?x)
{-# INLINE (!!?) #-}

dims :: Grid a -> (Int, Int)
dims = liftA2 (,) (V.length . V.head) V.length . _cells

subGrid :: Int -> Coord -> Vector Coord
subGrid width (V2 x y) = V.fromList
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
{-# INLINE minNeighbor #-}

minNeighbor' :: Ord a => Grid a -> Coord -> Tropical a
minNeighbor' g c = minimum $ Tropical . (g!!?) <$> potentialNeighbors c

neighbors :: Grid a -> Coord -> Vector Coord
neighbors g c = V.filter (\c -> isJust (g!!?c)) (potentialNeighbors c)

potentialNeighbors :: Coord -> Vector Coord
potentialNeighbors c = V.map (+c) $ V.fromList
  [ V2 (-1) (-1), V2 0 (-1), V2 1 (-1)
  , V2 (-1)   0 ,            V2 1   0
  , V2 (-1)   1 , V2 0   1 , V2 1   1  ]
{-# INLINE potentialNeighbors #-}

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
