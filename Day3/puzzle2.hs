main :: IO [Int]
main = do
  file <- readFile "puzzle.txt"
  let ls = lines file
  return (map bintodec [filterByCount ls (==), filterByCount ls (/=)])

getCountInPosition :: [[Char]] -> Int -> Char
getCountInPosition ls i =
  let bits = map (!! i) ls
      ones = length $ filter (== '1') bits
      zeroes = length ls - ones
   in if zeroes > ones then '0' else '1'

filterByCount :: [[Char]] -> (Char -> Char -> Bool) -> [Char]
filterByCount = filterByCount' 0
  where
    filterByCount' i ls eq =
      let count = getCountInPosition ls i
          filtered = filter (\line -> line !! i `eq` count) ls
       in if length filtered == 1 then head filtered else filterByCount' (i + 1) filtered eq

bintodec :: [Char] -> Int
bintodec = foldr f 0 . reverse
  where
    f '0' y = 2 * y
    f '1' y = 1 + 2 * y
    f _ y = error "Invalid"
