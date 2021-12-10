import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO Int
main = do
  file <- readFile "input.txt"
  let ls = lines file
      crabs = sort . map read . splitOn "," . head $ ls :: [Int]
      pos = crabs !! (length crabs `div` 2)
      fuel = sum $ map (abs . (pos -)) crabs
  return fuel