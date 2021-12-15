module Main where

import Data.Char (digitToInt)
import Data.Heap (Heap, MinHeap, filter, insert, singleton, view)
import Data.Maybe (fromJust)
import Debug.Trace (traceShowId)

data Q = Q Int (Int, Int) deriving (Show)

instance Eq Q where
  (==) (Q a _) (Q b _) = a == b

instance Ord Q where
  (<=) (Q a _) (Q b _) = a <= b

main = do
  file <- readFile "../input.txt"
  let ls = lines file
      grid = map (map digitToInt) ls :: [[Int]]
      repeated =
        map
          ( \r ->
              r
                ++ map (+ 1) r
                ++ map (+ 2) r
                ++ map (+ 3) r
                ++ map (+ 4) r
          )
          grid
      repeated' =
        repeated
          ++ map (map (+ 1)) repeated
          ++ map (map (+ 2)) repeated
          ++ map (map (+ 3)) repeated
          ++ map (map (+ 4)) repeated
      repeated'' = map (map (\x -> if x > 9 then x - 9 else x)) repeated'
  print (solvePuzzle repeated'')

solvePuzzle input =
  let grid = map (\x -> [999999] ++ x ++ [999999]) input
      row = map (const 999999) (head grid)
      grid' = [row] ++ grid ++ [row]
      dist = map (map (const 999999)) grid'
      dist' = set 0 (1, 1) dist
   in pathfind grid' (singleton (Q 0 (1, 1))) (last (sizeX grid') - 1, last (sizeY grid') - 1)

pathfind :: [[Int]] -> MinHeap Q -> (Int, Int) -> Int
pathfind grid queue end =
  let (Q pDist p, queue') = fromJust $ view queue
      grid' = set 999999 p grid
      q' = Data.Heap.filter (\(Q aDist a) -> a /= p) queue'
      --   q' = queue'
      neigh = neighbors p
      queue'' =
        foldl
          ( \q n ->
              if grid !!! n /= 999999
                then insert (Q (pDist + grid !!! n) n) q
                else q
          )
          q'
          neigh
   in if p == end then pDist else pathfind grid' queue'' end

minimumPoint :: [[Int]] -> (Int, Int)
minimumPoint dist =
  let min = minimum . Prelude.filter (>= 0) $ concat dist
   in head [(x, y) | x <- sizeX dist, y <- sizeY dist, (dist !!! (x, y)) == min]

(!!!) :: [[a]] -> (Int, Int) -> a
grid !!! (x, y) = (grid !! y) !! x

set :: a -> (Int, Int) -> [[a]] -> [[a]]
set val (x, y) = set' (set' (const val) x) y
  where
    set' f index list =
      let (x, e : y) = Prelude.splitAt index list
       in x ++ (f e : y)

sizeX grid = [0 .. (length (head grid) - 1)]

sizeY grid = [0 .. (length grid - 1)]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]