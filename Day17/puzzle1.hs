import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Debug.Trace (traceShowId)

main = do
  file <- readFile "input.txt"
  let (((_ : x : _) : (x2 : _) : _) : ((_ : y : _) : (y2 : _) : _) : _) =
        map (map (splitOn "=") . splitOn "..") $ splitOn ", " $ head $ lines file
      possible = [(x, y) | y <- reverse [1 .. 1000], x <- [0 .. 1000]]
      fire' = fire (0, 0) (read x, read y) (read x2, read y2)
  print (head . catMaybes $ map fire' possible)

fire (x, y) (x1, y1) (x2, y2) (dx, dy)
  | x1 <= x && x <= x2 && y1 <= y && y <= y2 = Just y
  | x2 < x || y < y2 = Nothing
  | otherwise = Just max <*> Just y <*> fire (x + dx, y + dy) (x1, y1) (x2, y2) (drag dx, dy -1)
  where
    drag dx
      | dx < 0 = dx + 1
      | dx > 0 = dx -1
      | otherwise = dx