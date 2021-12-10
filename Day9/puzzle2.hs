import Data.Char (digitToInt)
import Data.List (sort)

main = do
  file <- readFile "input.txt"
  let ls = lines file
      grid = map (\x -> [9] ++ map digitToInt x ++ [9]) ls :: [[Int]]
      row = map (const 9) (sizeX grid)
      grid' = [row] ++ grid ++ [row]
  return (product . take 3 . reverse . sort . map (basinSize grid') $ lowPoints grid')

lowPoints :: [[Int]] -> [(Int, Int)]
lowPoints grid = [(x, y) | x <- sizeX grid, y <- sizeY grid, isLowest (x, y)]
  where
    isLowest (x, y) = all ((> grid !!! (x, y)) . (grid !!!)) $ neighbors (x, y)

basinSize :: [[Int]] -> (Int, Int) -> Int
basinSize grid = length . filter (== 99) . concat . basinSize' grid
  where
    basinSize' grid (x, y) =
      foldl go grid
        . filter (\p -> grid !!! p /= 9 && grid !!! p /= 99)
        $ neighbors (x, y)
    go grid pos = basinSize' (set 99 pos grid) pos

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

sizeX :: [[Int]] -> [Int]
sizeX grid = [1 .. length (head grid) - 1]

sizeY :: [[Int]] -> [Int]
sizeY grid = [1 .. length grid - 2]

(!!!) :: [[a]] -> (Int, Int) -> a
grid !!! (x, y) = (grid !! y) !! x

set :: a -> (Int, Int) -> [[a]] -> [[a]]
set val (x, y) = set' (set' (const val) x) y
  where
    set' f index list =
      let (x, e : y) = splitAt index list
       in x ++ (f e : y)
