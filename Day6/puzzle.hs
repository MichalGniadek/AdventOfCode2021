import Data.List.Split (splitOn)

main :: IO Integer
main = do
  file <- readFile "input.txt"
  let ls = lines file
      start = map read . splitOn "," . head $ ls :: [Int]
      fishes = foldl (modify (+ 1)) (replicate 9 0) start
  return (sum $ simulate 256 fishes)

simulate :: Int -> [Integer] -> [Integer]
simulate 0 fishes = fishes
simulate n fishes =
  let fishes' = rotate 1 fishes
      fishes'' = modify (+ head fishes) fishes' 6
   in simulate (n - 1) fishes''

rotate :: Int -> [a] -> [a]
rotate n xs = take (length xs) (drop n (cycle xs))

modify :: (a -> a) -> [a] -> Int -> [a]
modify f l i =
  let (x, e : y) = splitAt i l
   in x ++ (f e : y)