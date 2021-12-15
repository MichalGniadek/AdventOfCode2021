import Data.Char (digitToInt)
import Debug.Trace (traceShowId)

main = do
  file <- readFile "input.txt"
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
  let grid = map (\x -> [999] ++ x ++ [999]) input
      row = map (const 999) (head grid)
      grid' = [row] ++ grid ++ [row]
      dist = map (map (const 999999)) grid'
      dist' = set 0 (1, 1) dist
   in pathfind grid' dist' (last (sizeX grid') - 1, last (sizeY grid') - 1)

pathfind grid dist end =
  let p = minimumPoint dist
      neigh = neighbors p
      dist' =
        [ [ if (x, y) `elem` neigh
              then min (dist !!! (x, y)) (dist !!! p + grid !!! (x, y))
              else
                if (x, y) == p
                  then - (dist !!! (x, y)) - 1
                  else dist !!! (x, y)
            | x <- sizeX grid
          ]
          | y <- sizeY grid
        ]
   in if p == end then - (dist' !!! end) - 1 else pathfind grid dist' end

minimumPoint :: [[Int]] -> (Int, Int)
minimumPoint dist =
  let min = minimum . filter (>= 0) $ concat dist
   in head [(x, y) | x <- sizeX dist, y <- sizeY dist, (dist !!! (x, y)) == min]

(!!!) :: [[a]] -> (Int, Int) -> a
grid !!! (x, y) = (grid !! y) !! x

set :: a -> (Int, Int) -> [[a]] -> [[a]]
set val (x, y) = set' (set' (const val) x) y
  where
    set' f index list =
      let (x, e : y) = splitAt index list
       in x ++ (f e : y)

sizeX grid = [0 .. (length (head grid) - 1)]

sizeY grid = [0 .. (length grid - 1)]

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + 1, y), (x -1, y), (x, y + 1), (x, y -1)]