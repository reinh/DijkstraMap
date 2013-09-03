{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- module GridFloyd where

import Prelude hiding      (concatMap, foldr, sum)
import Linear.V2
import Data.List           (intersect, nub)
import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Par
import Control.Applicative
import GHC.Arr             (range, inRange)
import Data.Vector         (Vector, (!),)

import qualified Data.Vector         as V
import qualified Data.Map            as M

instance NFData a => NFData (V2 a) where rnf (V2 a b) = rnf a `seq` rnf b `seq` ()

type Coord = V2 Int

newtype Grid a = Grid { _cells :: Vector (Vector a) }
  deriving (Show, Read, Functor, Foldable, Traversable)
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

bounds :: Grid a -> (Coord, Coord)
bounds (Grid xs) = (V2 0 0, V2 w h) where
  h = V.length xs - 1
  w = V.length (V.head xs) - 1

inBounds :: Grid a -> Coord -> Bool
inBounds = inRange . bounds

neighbors :: Grid a -> Coord -> Vector Coord
neighbors g v = V.filter (inBounds g) (potentialNeighbors v)

potentialNeighbors :: Coord -> Vector Coord
potentialNeighbors (V2 x y) = do
    x' <- V.enumFromTo (x-1) (x+1)
    y' <- V.enumFromTo (y-1) (y+1)
    guard $ (x,y) /= (x',y')
    return (V2 x' y')

gridFromList :: [[a]] -> Grid a
gridFromList = Grid . V.fromList . fmap V.fromList

showGrid :: Show a => Grid a -> String
showGrid = unlines . toList . fmap show . _cells

printGrid :: Show a => Grid a -> IO ()
printGrid = putStr . showGrid

data Cell = Floor | Wall deriving (Show, Read, Eq)

reachable :: Cell -> Bool
reachable = (==Floor)

reachables :: Grid Cell -> Grid Bool
reachables = fmap reachable

type Reachable = Grid Bool

gridVertices :: Grid a -> Vector Coord
gridVertices = join . _cells . imap const

type Weight = Int
type Graph = M.Map Coord (M.Map Coord Weight)

gridEdges :: Reachable -> Vector (Coord, Coord)
gridEdges g = do
    v <- gridVertices g
    e <- neighbors g v
    guard (g !!! e)
    return (v,e)

edgesToGraph' :: Vector (Coord, Coord) -> Graph
edgesToGraph' es = go (V.toList es) M.empty where
  go [] graph = graph
  go ((from, to):ess) graph =
    go ess (M.insertWith (<>) from (M.fromList [(to, 1)]) graph)

edgesToGraph :: Vector (Coord, Coord) -> Graph
edgesToGraph = V.foldl go M.empty where
  go :: Graph -> (Coord, Coord) -> Graph
  go g (from, to) = M.insertWith (<>) from (M.fromList [(to,1)]) g

gridToGraph :: Reachable -> Graph
gridToGraph = edgesToGraph . gridEdges

weight :: Graph -> Coord -> Coord -> Maybe Weight
weight g i j = do
    jmap <- M.lookup i g
    M.lookup j jmap

shortestPaths :: Reachable -> Graph
shortestPaths grid = V.foldl' update g vs
  where
    vs = gridVertices grid
    g = gridToGraph grid
    update g k = runPar $ do
      m <- M.traverseWithKey (\i jmap -> spawn (return (shortmap i jmap))) g
      traverse get m
        where
          shortmap :: Coord -> M.Map Coord Weight -> M.Map Coord Weight
          shortmap i jmap = V.foldr shortest M.empty vs
            where shortest j m =
                    case (old,new) of
                      (Nothing, Nothing) -> m
                      (Nothing, Just w ) -> M.insert j w m
                      (Just w, Nothing) -> M.insert j w m
                      (Just w1, Just w2) -> M.insert j (min w1 w2) m
                      where
                        old = M.lookup j jmap
                        new = do w1 <- weight g i k
                                 w2 <- weight g k j
                                 return (w1+w2)

main :: IO ()
main = print $ M.size (shortestPaths g) where
  g = gridFromList (replicate 10 (replicate 10 True))
