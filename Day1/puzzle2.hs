main :: IO Int
main = do
  file <- readFile "puzzle.txt"
  let ls = lines file
      hs = map read ls :: [Int]
      ws = zipWith3 (\x y z -> x + y + z) hs (tail hs) (tail (tail hs))
  return (inc ws)

inc :: [Int] -> Int
inc xs = length . filter id . tail $ zipWith (>) xs (0 : xs)