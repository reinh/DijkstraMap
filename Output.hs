{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Graphics.Vty
import Linear.V2
import Control.Lens hiding (Level)
import Debug.Trace
import Data.Word8

import qualified Data.Vector as V

import DijkstraMap
import Dijkstra.Grid (Grid(..), Reachable, reachableToWeighted, Weighted, Coord, (!!?), (!!!), printGrid)
import TropicalSemiring
import TestMaps

data Level = Level
    { _reachable :: Reachable
    , _player :: Coord
    } deriving (Show, Eq)

makeLenses ''Level

printLevel = putStrLn . showLevel

-- levelToImage :: Level -> Image
levelToImage level maps = img where
  r = _reachable level
  p = _player level
  distances = maps !!! p

  img = V.foldr (<->) empty_image lines
  lines = V.map go images where
    go line = (V.foldr (<|>) empty_image line) <|> char def_attr ' '

  Grid images = imap toImg r

  toImg i c = char (toAttr i) (toChar i c)

  toAttr i = case (distances !!! i) of
    Tropical Nothing -> def_attr `with_back_color` black `with_fore_color` black
    Tropical (Just n) -> def_attr `with_back_color` colorFor n

  colorFor n | n > 21 = black
             | n == 0 = white
             | otherwise = Color240 $ fromIntegral $ 224 - n + 1

  toChar c _ | c == p = '@'
  toChar _ True = '.'
  toChar _ False = '#'

showLevel :: Level -> String
showLevel level = str where
  str = unlines $ V.toList $ V.map V.toList $ _cells (imap toChar r)

  r = _reachable level
  p = _player level

  toChar c _ | c == p = '@'
  toChar _ True = '.'
  toChar _ False = '#'

levelToDistances :: Level -> Weighted
levelToDistances (Level g c) = resolve c (reachableToWeighted g)


main :: IO ()
main = do
    let level = Level testDungeon (V2 1 1) 
        maps = syncBuildGrids (reachableToWeighted $ _reachable level)
    fs <- mkVty
    go fs maps level

testColors :: Vty -> IO ()
testColors fs = update fs $ pic_for_image $ foldl f empty_image colors
  where
    colors = [200..255] :: [Word8]
    f :: Image -> Word8 -> Image
    f acc c = acc <-> colored c
    colored c = string (def_attr `with_back_color` (Color240 c)) (show c)

-- go :: Vty -> Level -> IO()
go fs maps level = do
    update fs $ pic_for_image $ levelToImage level maps
    e <- next_event fs
    case e of
      EvKey (KASCII 'h') [] -> go fs maps $ move level (V2 (-1)  0)
      EvKey (KASCII 'j') [] -> go fs maps $ move level (V2   0   1)
      EvKey (KASCII 'k') [] -> go fs maps $ move level (V2   0 (-1))
      EvKey (KASCII 'l') [] -> go fs maps $ move level (V2   1   0)
      otherwise -> shutdown fs

move :: Level -> Coord -> Level
move level c =
  let
    r = level^.reachable
    newCoord = level^.player + c
  in
    case (r !!? newCoord) of
      Nothing -> level
      Just False -> level
      Just True -> level & player .~ newCoord
