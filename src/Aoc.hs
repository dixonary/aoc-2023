{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}
module Aoc where

import Witch

import Utils.Parsers as P
import Utils.Utils as U

import Data.Function
import Data.List (find, foldl', isPrefixOf)

import Control.Applicative.Combinators
import Control.Arrow ((>>>))
import Data.Attoparsec.Text hiding (choice, count, sepBy, sepBy1, take)
import Data.Bifunctor
import Data.Either
import Data.Functor
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T

import Data.Char
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Product (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import Debug.Trace
import Prelude hiding (takeWhile)

--------------------------------------------------------------------------------
-- DAY 01

parse01 :: String -> [([(Bool, Integer)], [(Bool, Integer)])]
parse01 xs =
  xs
    & lines
    & map (\x -> (pparse numLeft x, pparse numRight (reverse x)))
 where
  num ns = catMaybes <$> many (numeral ns)
  numLeft = num numerals
  numRight = num (map (first T.reverse) numerals)

  numerals :: [(T.Text, Integer)]
  numerals =
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]

  numeral ns =
    choice [string n $> Just (False, x) | (n, x) <- ns]
      <|> (Just . (True,) . read . pure <$> digit)
      <|> (satisfy (/= '\n') $> Nothing)

sumNumerals strs xs =
  xs
    & map (both $ fmap snd . filter strs)
    & map (\(x : _, y : _) -> x * 10 + y)
    & sum

day01a :: [([(Bool, Integer)], [(Bool, Integer)])] -> Integer
day01a = sumNumerals fst

day01b :: [([(Bool, Integer)], [(Bool, Integer)])] -> Integer
day01b = sumNumerals (const True)

--------------------------------------------------------------------------------
-- DAY 02

parse02 :: Parser [(Integer, [Map Text Integer])]
parse02 = game `sepBy` endOfLine
 where
  game =
    (,)
      <$> (string "Game " *> decimal <* string ": ")
      <*> (set `sepBy` string "; ")
  set = do
    ms <-
      ((,) <$> decimal <*> (skipSpace *> takeWhile isAlpha))
        `sepBy` string ", "
    pure $ Map.fromList (map swap ms)

day02a :: [(Integer, [Map Text Integer])] -> Integer
day02a = sum . map fst . filter (goodBalls . Map.unionsWith max . snd)
 where
  goodBalls m = goodRed && goodGreen && goodBlue
   where
    goodRed = Map.lookup "red" m & maybe True (<= 12)
    goodGreen = Map.lookup "green" m & maybe True (<= 13)
    goodBlue = Map.lookup "blue" m & maybe True (<= 14)

day02b :: [(Integer, [Map Text Integer])] -> Integer
day02b = sum . map (product . Map.unionsWith max . snd)

--------------------------------------------------------------------------------
-- DAY 03

data SchLabel = Part Char | Digit Integer deriving (Eq)

parse03 :: Parser (Map (Int, Int) SchLabel)
parse03 = coordinateParser mapper 0
 where
  mapper c
    | c == '.' = Nothing
    | c `elem` ['0' .. '9'] = Just $ Digit (read [c])
    | otherwise = Just $ Part c

day03a :: Map (Int, Int) SchLabel -> Integer
day03a m =
  Map.keysSet partPositions
    & Set.map neighbours8
    & Set.unions
    & Set.map (flood m)
    & Set.toList
    & map (foldl' (\v a -> let Digit n = m Map.! a in v * 10 + n) 0)
    & sum
 where
  partPositions = Map.filter (\case Part _ -> True; _ -> False) m

flood m (x, y) = case Map.lookup (x, y) m of
  Nothing -> Set.empty
  Just (Part n) -> Set.empty
  Just (Digit n) ->
    let flood' = flood (Map.delete (x, y) m)
     in flood' (x - 1, y) <> Set.singleton (x, y) <> flood' (x + 1, y)

day03b :: Map (Int, Int) SchLabel -> Integer
day03b m =
  starPositions
    & Set.map neighbours8
    & Set.unions
    & Set.map (flood m)
    & (\ns -> map (\p -> Set.filter (isNeighbour p) ns) $ Set.toList starPositions)
    & filter (\s -> Set.size s == 2)
    & map Set.toList
    & map (map (foldl' (\v a -> let Digit n = m Map.! a in v * 10 + n) 0))
    & map product
    & sum
 where
  starPositions = Map.keysSet $ Map.filter (== Part '*') m

  twoNeighbours ns starPosition =
    Set.size (Set.filter (any (`Set.member` neighbours8 starPosition)) ns) == 2

  isNeighbour p = any (`Set.member` neighbours8 p)

--------------------------------------------------------------------------------
-- DAY 04

parse04 :: Parser (Map Integer (Set Integer, Set Integer))
parse04 = Map.fromList <$> card `sepBy` endOfLine
 where
  card = do
    string "Card"
    skipSpace
    n <- decimal
    char ':'
    ns <- (skipSpace *> decimal) `sepBy` skipSpace `around` string " |"
    pure (n, both Set.fromList ns)

day04a :: Map Integer (Set Integer, Set Integer) -> Integer
day04a =
  sum
    . fmap
      ( (\case 0 -> 0; n -> 2 ^ pred n)
          . Set.size
          . uncurry Set.intersection
      )

day04b :: Map Integer (Set Integer, Set Integer) -> Integer
day04b m = sum . foldl' f (fmap (const 1) m) . Map.keys $ m
 where
  f mm ix =
    let
      wins = Set.size . uncurry Set.intersection $ m Map.! ix
      score = (\case 0 -> 0; w -> 2 ^ pred w) wins
      copies = mm Map.! ix
     in
      foldl' (flip (Map.adjust (+ copies))) mm [ix + 1 .. ix + from wins]