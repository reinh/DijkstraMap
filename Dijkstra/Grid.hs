{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Dijkstra.Grid (
  -- Grids of (tropical) weights
    Weighted
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

import Debug.Trace

import Prelude hiding (concat, maximum, minimum)

import Control.Applicative (Applicative(..), pure, liftA2)
import Control.DeepSeq     (NFData(..))
import Control.Monad       (guard)
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
import Dijkstra.Grid.Vector


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
