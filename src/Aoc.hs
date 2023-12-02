{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use map once" #-}
module Aoc where

import Witch

import Utils.Parsers as P
import Utils.Utils as U

import Data.Function
import Data.List (find, isPrefixOf)

import Control.Applicative.Combinators (
  between,
  count,
  many,
  sepBy,
  sepBy1,
  (<|>),
 )
import Control.Arrow ((>>>))
import Data.Attoparsec.Text hiding (count, sepBy, sepBy1, take)
import Data.Bifunctor
import Data.Either
import Data.Functor
import Data.Maybe (catMaybes)
import Data.Text qualified as T

parse01 :: String -> [([(Bool, Integer)], [(Bool, Integer)])]
parse01 xs =
  xs
    & lines
    & map (\x -> (x, reverse x))
    & map (bimap (pparse numLeft) (pparse numRight))
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
