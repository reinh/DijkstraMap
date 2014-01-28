{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}

module Dijkstra.Grid.Array (
  -- Basic grids
    Grid(..)
  , (!!!), (!!?)
  , update, modify
  , inBounds, dims
  , gridFromList

  -- Neighbors
  , minNeighbor

  -- Resolving
  , resolveOnce

  -- Printing and showing
  , showGrid, printGrid
  ) where

import Linear.V2

import Prelude hiding (concat, maximum, minimum)

import Data.Foldable   (Foldable(..), concat, maximum, minimum)
import Control.Applicative
import Data.List.Split (chunksOf)
import Control.DeepSeq (NFData(..))

import Control.Lens
import Control.Monad

import Data.Array
import Data.Array.ST

import qualified Data.Vector as V

import Dijkstra.Coord
import Dijkstra.Tropical
import Data.List(transpose)

newtype Grid a = Grid { _cells :: Array Coord a }
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

instance NFData a => NFData (Grid a) where
    rnf g = rnf (_cells g) `seq` ()

-- makeLenses ''Grid
-- makePrisms ''Grid

(!!!) :: Grid a -> Coord -> a
(Grid cs) !!! c = cs ! c

(!!?) :: Grid a -> Coord -> Maybe a
g !!? c | inBounds g c = Just (g !!! c)
        | otherwise    = Nothing

update ::  Grid a -> Coord -> a -> Grid a
update (Grid cs) c x = Grid (cs // [(c, x)])

modify ::  Grid a -> Coord -> (a -> a) -> Grid a
modify (Grid cs) c f = Grid cs' where
  cs' = cs // [(c, f (cs ! c))]

inBounds :: Grid a -> Coord -> Bool
inBounds (Grid cs) c = inRange (bounds cs) c

dims :: Grid a -> (Int, Int)
dims (Grid cs) = (upper^._x, upper^._y) where
  upper = (bounds cs)^._2

gridFromList :: [[a]] -> Grid a
gridFromList []   = Grid $ listArray (V2 0 0, V2 0 0) []
gridFromList [[]] = Grid $ listArray (V2 0 0, V2 0 0) []
gridFromList xs   = Grid $ listArray (V2 0 0, V2 w h) (concat xs) where
  w = length xs - 1
  h = length (head xs) - 1

-- Neighbors

minNeighbor :: Ord a => Grid a -> Coord -> Tropical a
minNeighbor g c = minimum $ Tropical . (g!!?) <$> potentialNeighbors c
{-# INLINE minNeighbor #-}

potentialNeighbors :: Coord -> [Coord]
potentialNeighbors c = fmap (+c) [
    V2 (-1) (-1), V2 0 (-1), V2 1 (-1)
  , V2 (-1)   0 ,            V2 1   0
  , V2 (-1)   1 , V2 0   1 , V2 1   1  ]
{-# INLINE potentialNeighbors #-}


-- Resolving

resolveOnce :: Coord -> V.Vector Coord -> Grid Weight -> Grid Weight
resolveOnce start order (Grid cells) = Grid $ runSTArray $ do
    cells' <- thaw cells
    bounds' <- getBounds cells'
    let inBounds' = inRange bounds'

    -- mark the starting cell as the target
    writeArray cells' start (pure 0)

    -- update cells in order
    V.forM_ order $ \c -> do
      current <- readArray cells' c
      neighbors <- filter inBounds' (potentialNeighbors c) `forM` readArray cells'
      writeArray cells' c (current `min` minimum neighbors + 1)

    return cells'

-- Helpers

printGrid :: Show a => Grid a -> IO ()
printGrid = putStrLn . showGrid

showGrid :: Show a => Grid a -> String
showGrid (Grid cs) = unlines . fmap concat . transpose . chunksOf w $ padded where
  padded = elems $ fmap (padL mx) shown
  mx = 1 + maximum (fmap length shown)
  shown = fmap show cs
  w = (bounds cs)^._2._y + 1

padL ::  Int -> [Char] -> String
padL n s = let extra = n - (length s) in pad extra s where
  pad :: Int -> String -> String
  pad 0  s'   = s'
  pad 1  s'   = ' ':s'
  pad n' s'
    | n' < 0 = s'
    | n' > 1 = ' ':pad (n'-1) s'
  pad _  s'   = s'
