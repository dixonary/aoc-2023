{-# LANGUAGE MultiWayIf #-}

module Utils.Utils where

import Data.Map (Map)
import Data.Map qualified as Map

import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Debug.Trace

{-
This module contains a series of miscellaneous utility functions that Sam has found helpful in the past.
-}

-- Takes a list.
-- Returns a map from elements of that list to the number of times they appeared in the list.
freq :: (Ord a) => [a] -> Map a Int
freq = Map.fromListWith (+) . fmap (,1)

-- Takes a nested list (to be thought of as a 2D structure).
-- Returns a map from "co-ordinates" to the items in the list.
-- For example:
--     Input: [[a,b,c],[d,e]]
--     Output: Map.fromList [((0,0),a), ((0,1),b), ((0,2),c), ((1,0),d), ((1,1),e)]
mapFromNestedLists :: (Ord a) => [[a]] -> Map (Int, Int) a
mapFromNestedLists = Map.fromList . attachCoords 0 0
 where
  attachCoords _ _ [] = []
  attachCoords x _ ([] : ls) = attachCoords (x + 1) 0 ls
  attachCoords x y ((l : ls) : lss) = ((x, y), l) : attachCoords x (y + 1) (ls : lss)

-- Splits a list into maximal contiguous chunks that satisfy the given predicate.
-- For example:
--     Input: (> 3) [5,4,3,2,7,6,3,4]
--     Output: [[5,4],[7,6],[4]]
chunksByPredicate :: (a -> Bool) -> [a] -> [[a]]
chunksByPredicate p ls
  | null ls = []
  | otherwise =
      let (prefix, rest) = span p ls
       in if null prefix
            then chunksByPredicate p $ dropWhile (not . p) rest
            else prefix : chunksByPredicate p (dropWhile (not . p) rest)

-- Allows the user to log out some context and then the result of some expression
-- For example, supposing a is 2, and b is 5:
--     Input: traceShowIdWithContext (a, b) $ a + b
--     Output: (2, 5)	7
traceShowIdWithContext :: (Show a, Show b) => a -> b -> b
traceShowIdWithContext context result = trace (show context ++ "\t" ++ show result) result

-- Like !!, but with bounds checking
(!!?) :: [a] -> Int -> Maybe a
list !!? index =
  if
    | index < 0 -> Nothing
    | index >= length list -> Nothing
    | otherwise -> Just $ list !! index

-- Given a map where the keys are co-ordinates, returns the minimum x, maximum x, minimum y, and maximum y; in that order.
mapBoundingBox :: Map (Int, Int) a -> (Int, Int, Int, Int)
mapBoundingBox m =
  (,,,)
    (minimum . fmap fst . Map.keys $ m)
    (maximum . fmap fst . Map.keys $ m)
    (minimum . fmap snd . Map.keys $ m)
    (maximum . fmap snd . Map.keys $ m)

-- Given a rectangular, filled map, returns a list of all the coordinates hit by some (dx,dy) ray.
mapRay :: Map (Int, Int) a -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
mapRay m (x, y) (dx, dy) =
  let (minX, maxX, minY, maxY) = mapBoundingBox m
   in [ (x', y')
      | x' <- if dx == 0 then repeat x else [x, x + dx .. if dx > 0 then maxX else minX]
      | y' <- if dy == 0 then repeat y else [y, y + dy .. if dy > 0 then maxY else minY]
      ]

(...) :: (Num a, Ord a, Enum a) => a -> a -> [a]
(...) x y
  | x <= y = [x .. y]
  | otherwise = [y .. x]

(|+|) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x, y) |+| (u, v) = (x + u, y + v)

(|-|) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(x, y) |-| (u, v) = (x - u, y - v)

(|*|) :: (Int, Int) -> Int -> (Int, Int)
(x, y) |*| k = (k * x, k * y)

diff :: (Num a) => a -> a -> a
x `diff` y = abs $ x - y

(!@) :: (Ord k, Monoid a) => Map k a -> k -> a
m !@ k = fromMaybe mempty $ Map.lookup k m

-- Repeat an operation until the inputs do not change
converge :: (Eq a) => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

-- Apply a function to both elements of a pair
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- Get all the neighbours of a point...

-- ... orthogonally
neighbours4 (x, y) =
  Set.fromList [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- ... and diagonally
neighbours8 (x, y) =
  Set.fromList
    [ (x', y')
    | x' <- [x - 1 .. x + 1]
    , y' <- [y - 1 .. y + 1]
    , (x', y') /= (x, y)
    ]

numerate :: (Foldable t) => t Integer -> Integer
numerate = List.foldl' (\v a -> v * 10 + a) 0

with :: (a -> b) -> a -> (b, a)
with f a = (f a, a)
