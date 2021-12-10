import Data.Char (digitToInt)

main :: IO Int
main = do
  file <- readFile "input.txt"
  let ls = lines file
      grid = map (\x -> [9] ++ map digitToInt x ++ [9]) ls :: [[Int]]
      row = map (const 9) (sizeX grid)
      grid' = [row] ++ grid ++ [row]
  return (sum . map ((+ 1) . (grid' !!!)) $ lowPoints grid')

lowPoints grid = [(x, y) | x <- sizeX grid, y <- sizeY grid, isLowest (x, y)]
  where
    isLowest (x, y) = all ((> (grid !!! (x, y))) . (grid !!!)) (neighbors (x, y))

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

sizeX grid = [1 .. length (head grid) - 1]

sizeY grid = [1 .. length grid - 2]

(!!!) :: [[a]] -> (Int, Int) -> a
grid !!! (x, y) = (grid !! y) !! x