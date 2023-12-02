{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}
module Aoc where

import Witch

import Utils.Parsers as P
import Utils.Utils as U

import Data.Function
import Data.List (find, isPrefixOf)

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
import Data.Tuple (swap)
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
  game = do
    g <- string "Game " *> decimal
    string ": "
    s <- set `sepBy` string "; "
    pure (g, s)
  set = do
    ms <- ((,) <$> decimal <*> (skipSpace *> takeWhile isAlpha)) `sepBy` string ", "
    pure $ Map.fromList (map swap ms)

day02a :: [(Integer, [Map Text Integer])] -> Integer
day02a xs =
  xs
    & filter (not . any tooManyBalls . snd)
    & map fst
    & sum
 where
  tooManyBalls m = tooManyRed || tooManyGreen || tooManyBlue
   where
    tooManyRed = Map.lookup "red" m & maybe False (> 12)
    tooManyGreen = Map.lookup "green" m & maybe False (> 13)
    tooManyBlue = Map.lookup "blue" m & maybe False (> 14)

day02b :: [(Integer, [Map Text Integer])] -> Integer
day02b = sum . map (product . Map.unionsWith max . snd)