import Data.HashMap.Lazy (HashMap, empty, insertWith)
import Data.List.Split (splitOn)

main :: IO Int
main = do
  file <- readFile "puzzle.txt"
  let ls = map parseLine (lines file)
      points = concatMap linePoints ls
      m = empty :: HashMap (Int, Int) Int
      filled = foldl (\m p -> insertWith (+) p 1 m) m points
  return (foldl (\acc e -> if e > 1 then acc + 1 else acc) 0 filled)

type Line = (Point, Point)

parseLine :: String -> Line
parseLine l = (parsePoint (head ws), parsePoint (last ws))
  where
    ws = words l

type Point = (Int, Int)

parsePoint :: String -> Point
parsePoint p = (head coords, last coords)
  where
    coords = map read $ splitOn "," p

linePoints :: Line -> [Point]
linePoints ((x0, y0), (x1, y1)) =
  if (x0 == x1) || (y0 == y1)
    then [(x, y) | x <- range x0 x1, y <- range y0 y1]
    else zip (range x0 x1) (range y0 y1)

range :: Int -> Int -> [Int]
range a b = if a < b then [a .. b] else reverse [b .. a]