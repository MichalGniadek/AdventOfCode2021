main :: IO Int
main = do
  file <- readFile "puzzle.txt"
  let ls = lines file
      hs = map read ls :: [Int]
  return (inc hs)

inc :: [Int] -> Int
inc xs = length . filter id . tail $ zipWith (>) xs (0 : xs)