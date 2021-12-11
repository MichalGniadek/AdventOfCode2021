import Data.Char (digitToInt)
import Debug.Trace (traceShowId)

main = do
  file <- readFile "input.txt"
  let ls = lines file
      grid = map (\x -> [100, 100] ++ map digitToInt x ++ [100, 100]) ls :: [[Int]]
      row = map (const 100) (sizeX grid)
      grid' = [row] ++ grid ++ [row]
  return (loop (0, grid') 100)

loop grid 0 = grid
loop grid n =
  let grid' = increaseEnergy grid
   in loop (resetEnergy $ foldl tryToFlash grid' (highEnergy grid')) (n -1)

increaseEnergy (flashes, grid) = (flashes, map (map (+ 1)) grid)

resetEnergy (flashes, grid) = (flashes, [[reset $ grid !!! (x, y) | x <- sizeX grid] | y <- sizeY grid])
  where
    reset val = if val > 999 then 0 else val

tryToFlash (flashes, grid) pos =
  if grid !!! pos < 10 || grid !!! pos > 99
    then (flashes, grid)
    else
      let grid' = set 1000 pos grid
       in foldl handleNeighbor (flashes + 1, grid') (neighbors pos)

handleNeighbor (flashes, grid) pos =
  let grid' = set (grid !!! pos + 1) pos grid
   in tryToFlash (flashes, grid') pos

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1], i /= 0 || j /= 0]

highEnergy (_, grid) = [(x, y) | x <- sizeX grid, y <- sizeY grid, grid !!! (x, y) > 9]

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