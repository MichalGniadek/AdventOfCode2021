main :: IO (Int, Int)
main = do
  file <- readFile "puzzle.txt"
  let ls = lines file
      c = replicate (length (head ls)) (Count 0 0)
      f = foldl count c ls
      fin = unzip $ map conv f
  return (bintodec (fst fin), bintodec (snd fin))

data Count = Count Int Int deriving (Show)

count :: [Count] -> [Char] -> [Count]
count = zipWith combine
  where
    combine (Count z o) d = if d == '0' then Count (z + 1) o else Count z (o + 1)

conv :: Count -> (Char, Char)
conv (Count z o)
  | z > o = ('0', '1')
  | otherwise = ('1', '0')

bintodec :: [Char] -> Int
bintodec = foldr f 0 . reverse
  where
    f '0' y = 2 * y
    f '1' y = 1 + 2 * y
    f _ y = error "Invalid"
