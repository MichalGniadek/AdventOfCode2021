import Data.Char (isUpper)
import Data.List.Split (splitOn)

main = do
  file <- readFile "input.txt"
  let edges = map ((\x -> (head x, x !! 1)) . splitOn "-") (lines file)
      con = edges ++ map (\(x, y) -> (y, x)) edges
  print (paths con [] "start")

paths con visited "end" = 1
paths con visited cave
  | cave `elem` visited = 0
  | otherwise =
    let neigh = map snd $ filter ((== cave) . fst) con
        visited' = if isUpper (head cave) then visited else cave : visited
     in sum $ map (paths con visited') neigh