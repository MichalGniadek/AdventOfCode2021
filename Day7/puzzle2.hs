import Data.List.Split (splitOn)

main = do
  file <- readFile "input.txt"
  let ls = lines file
      crabs = map read . splitOn "," . head $ ls :: [Int]
      pos = sum crabs `div` length crabs
      fuel1 = sum $ map (fuel . abs . (pos -)) crabs
      fuel2 = sum $ map (fuel . abs . ((pos + 1) -)) crabs
  return (min fuel1 fuel2)

fuel :: Int -> Int
fuel dist = (1 + dist) * dist `div` 2