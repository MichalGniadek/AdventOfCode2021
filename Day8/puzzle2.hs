{-# LANGUAGE TupleSections #-}

import Data.List (elemIndex, intersect, sort, union)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

main :: IO Int
main = do
  file <- readFile "input.txt"
  return
    ( sum
        . map
          ( decode
              . (\x -> (words $ head x, words $ last x))
              . splitOn " | "
          )
        . lines
        $ file
    )

-- aaaa
--b    c
--b    c
-- dddd
--e    f
--e    f
-- gggg

decode :: ([String], [String]) -> Int
decode (patterns, output) =
  let _1 = count . head $ filterLen 2 patterns
      _7 = count . head $ filterLen 3 patterns
      a = fst . head . filterCount 1 $ combine _1 _7

      _4 = count $ head $ filterLen 4 patterns
      _235 = map count $ filterLen 5 patterns
      dg = filterNotChar a . filterCount 3 $ foldl1 combine _235
      dg' = combine _4 (count $ map fst dg)
      d = fst . head $ filterCount 2 dg'
      g = fst . head $ filterNotChar d dg

      be = filterCount 1 $ foldl1 combine _235
      be' = combine _4 (count $ map fst be)
      b = fst . head $ filterCount 2 be'
      e = fst . head $ filterNotChar b be

      _2 = head $ filter (elem (e, 1)) _235
      _3 = head . filter (notElem (b, 1)) $ filter (notElem (e, 1)) _235
      _5 = head $ filter (elem (b, 1)) _235

      _0 = head . filter (notElem (d, 1)) . map count $ filterLen 6 patterns
      _6 = head . filter (elem (e, 1)) . filter (elem (d, 1)) . map count $ filterLen 6 patterns
      _9 = head . filter (notElem (e, 1)) . map count $ filterLen 6 patterns

      _8 = count $ head $ filterLen 7 patterns
      nums = map (map fst . filterCount 1) [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9]
      output' = map sort output
   in foldl1 (\s n -> s * 10 + n) $ map (fromJust . (`elemIndex` nums)) output'

filterLen :: Int -> [String] -> [String]
filterLen n = filter (\x -> length x == n)

filterCount :: Int -> [(Char, Int)] -> [(Char, Int)]
filterCount n = filter (\x -> snd x == n)

filterNotChar :: Char -> [(Char, Int)] -> [(Char, Int)]
filterNotChar ch = filter (\x -> fst x /= ch)

count :: String -> [(Char, Int)]
count = foldl (\count s -> map (\(c, i) -> if c == s then (c, i + 1) else (c, i)) count) empty
  where
    empty = map (,0) $ sort "abcdefg"

combine :: [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
combine = zipWith (\(a, i) (b, n) -> (a, i + n))