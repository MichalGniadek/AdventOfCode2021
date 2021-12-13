import Data.List (nub)
import Data.List.Split (splitOn)

main = do
  file <- readFile "input.txt"
  let ls = splitOn [""] $ lines file
      dots = map ((\[x, y] -> (read x, read y)) . splitOn ",") (head ls) :: [(Int, Int)]
      folds = map ((\[x, y] -> (x, read y :: Int)) . splitOn "=" . (!! 2) . words) (last ls)
  putStr (showPaper $ nub $ foldl foldPaper dots folds)

foldPaper dots ("x", f) = map (\(x, y) -> (f - abs (f - x), y)) dots
foldPaper dots ("y", f) = map (\(x, y) -> (x, f - abs (f - y))) dots
foldPaper dots (_, f) = error "wrong fold direction"

showPaper :: [(Int, Int)] -> String
showPaper dots =
  let maxX = maximum $ map fst dots
      maxY = maximum $ map snd dots

      arr = [[if (x, y) `elem` dots then '█' else '.' | x <- [0 .. maxX]] | y <- [0 .. maxY]]
   in unlines arr