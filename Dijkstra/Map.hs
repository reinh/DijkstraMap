{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}

module Dijkstra.Map where

import Dijkstra.BFS
import Dijkstra.Coord
import Dijkstra.Grid

goFixed :: Eq a => (a -> a) -> a -> a
goFixed f g | g' == g  = g
            | otherwise = goFixed f g' where
              g' = f g

-- Start by resolving the map twice (which is sufficient for many cases),
-- then continue searching for a fixed point by repeatedly resolving the map.
resolve :: Coord -> Weighted -> Weighted
resolve start grid = goFixed resolve' grid where
  resolve' g = resolveOnce start order g
  order = bfsOrder grid start
