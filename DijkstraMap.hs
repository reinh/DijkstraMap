{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}

module Main where

import TropicalSemiring

import Prelude hiding      (concatMap, foldr, sum, maximum, minimum, concat, mapM_)
import Linear.V2
import Data.List           (intersect, nub)
import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.DeepSeq
import Control.Lens
import Control.Monad hiding (mapM_)
import Control.Monad.Par
import Control.Applicative
import Control.Parallel
import Control.Parallel.Strategies
import GHC.Arr             (range, inRange)
import Data.Vector         (Vector, (!),)
import Data.Vector.Strategies

import qualified Data.Vector         as V
import qualified Data.Map            as M

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

(!!!) :: Grid a -> Coord -> a
grid !!! v = grid^?!ix v

(!!?) :: Grid a -> Coord -> Maybe a
grid !!? v = grid^?ix v

resolve :: Weighted -> Weighted
resolve g | step g == g = g
          | otherwise   = resolve (step g)

step :: Weighted -> Weighted
step g = let f' = f g in imap f' g where

f g i v = update <$> v <*> m where
  update v m = min v (m + 1)
  m = join (minNeighbor g i)

minNeighbor :: Ord a => Grid a -> Coord -> Tropical a
minNeighbor g c = minimum $ Tropical . (g!!?) <$> potentialNeighbors c where
  potentialNeighbors :: Coord -> Vector Coord
  potentialNeighbors (V2 x y) = do
      x' <- V.enumFromTo (x-1) (x+1)
      y' <- V.enumFromTo (y-1) (y+1)
      guard $ (x,y) /= (x',y')
      return (V2 x' y')

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

gridFromList :: [[a]] -> Grid a
gridFromList = Grid . V.fromList . fmap V.fromList

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

instance Show a => Show (Tropical a) where
    show (Tropical Nothing)  = "∞"
    show (Tropical (Just a)) = show a

buildMaps :: Weighted -> M.Map Coord Weighted
buildMaps g = runPar $ do
  m <- traverse (spawnP . resolve) (targetted g)
  traverse get m

asyncBuildMaps g = syncBuildMaps g `using` parTraversable rpar
syncBuildMaps g = fmap resolve (targetted g)

target :: Weighted -> Coord -> Weighted
target g c = g & ix c .~ pure 0

targetted :: Weighted -> M.Map Coord Weighted
targetted g = M.fromList $ gs^@..itraversed where gs = imap (\i _ -> g `target` i) g

-- main :: IO ()
main = do
    let m = asyncBuildMaps testMap
    m `deepseq` print "done"
