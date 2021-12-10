{-# LANGUAGE TupleSections #-}

import Data.List (transpose, (\\))
import Data.List.Split (splitOn)
import Debug.Trace (traceShow, traceShowId)

main :: IO Int
main = do
  file <- readFile "puzzle.txt"
  let ls = lines file
      moves = map read (splitOn "," (head ls)) :: [Int]
      boards =
        parseBoards
          . map (map read . words)
          . filter (/= "")
          . tail
          $ ls
  return (run moves boards)

type Board = [[(Int, Bool)]]

parseBoards :: [[Int]] -> [Board]
parseBoards [] = []
parseBoards lines = board : parseBoards (drop 5 lines)
  where
    board = map (map (,False)) . take 5 $ lines

run :: [Int] -> [Board] -> Int
run moves boards =
  let marked = mark (head moves) boards
      won = winning marked
      remaining = marked \\ won
   in if null remaining then head moves * score (head won) else run (tail moves) remaining

mark :: Int -> [Board] -> [Board]
mark num = map (map (map (\(n, b) -> (n, b || num == n))))

score :: Board -> Int
score = sum . map (sum . map fst . filter (not . snd))

winning :: [Board] -> [Board]
winning boards = flt boards ++ (map transpose . flt . map transpose $ boards)
  where
    flt = filter (any (all snd))