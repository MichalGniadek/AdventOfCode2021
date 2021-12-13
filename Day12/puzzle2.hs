import Data.Char (isUpper)
import Data.List.Split (splitOn)

main = do
  file <- readFile "input.txt"
  let edges = map ((\x -> (head x, x !! 1)) . splitOn "-") (lines file)
      con = edges ++ map (\(x, y) -> (y, x)) edges
  print (paths con [] False "start")

paths con visited twice "end" = 1
paths con visited twice cave
  | cave `elem` visited && (twice || cave == "start") = 0
  | cave `elem` visited =
    let neigh = map snd $ filter ((== cave) . fst) con
        visited' = if isUpper (head cave) then visited else cave : visited
     in sum $ map (paths con visited' True) neigh
  | otherwise =
    let neigh = map snd $ filter ((== cave) . fst) con
        visited' = if isUpper (head cave) then visited else cave : visited
     in sum $ map (paths con visited' twice) neigh