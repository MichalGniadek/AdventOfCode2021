import Data.List.Split (splitOn)

main = do
  file <- readFile "input.txt"
  let ls = lines file
      output = length . filter correct $ concatMap (words . last . splitOn " | ") ls
  return output

correct :: String -> Bool
correct s = n == 2 || n == 3 || n == 4 || n == 7
  where
    n = length s