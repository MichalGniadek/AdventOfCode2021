import Data.Char (digitToInt)
import Debug.Trace (traceShowId)

main = do
  file <- readFile "input.txt"
  let ls = lines file
      grid = map (\x -> [100, 100] ++ map digitToInt x ++ [100, 100]) ls :: [[Int]]
      row = map (const 100) (sizeX grid)
      grid' = [row] ++ grid ++ [row]
  return (loop grid' 0)

loop grid n =
  let grid' = increaseEnergy grid
      afterFlash = foldl tryToFlash grid' (highEnergy grid')
   in if all (all (> 99)) afterFlash
        then n + 1
        else loop (resetEnergy afterFlash) (n + 1)

increaseEnergy = map (map (+ 1))

resetEnergy grid = [[reset $ grid !!! (x, y) | x <- sizeX grid] | y <- sizeY grid]
  where
    reset val = if val > 999 then 0 else val

tryToFlash grid pos =
  if grid !!! pos < 10 || grid !!! pos > 99
    then grid
    else
      let grid' = set 1000 pos grid
       in foldl handleNeighbor grid' (neighbors pos)

handleNeighbor grid pos =
  let grid' = set (grid !!! pos + 1) pos grid
   in tryToFlash grid' pos

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]

highEnergy grid = [(x, y) | x <- sizeX grid, y <- sizeY grid, grid !!! (x, y) > 9]

sizeX grid = [0 .. length (head grid) - 1]

sizeY grid = [0 .. length grid - 1]

(!!!) :: [[a]] -> (Int, Int) -> a
grid !!! (x, y) = (grid !! y) !! x

set :: a -> (Int, Int) -> [[a]] -> [[a]]
set val (x, y) = set' (set' (const val) x) y
  where
    set' f index list =
      let (x, e : y) = splitAt index list
       in x ++ (f e : y)