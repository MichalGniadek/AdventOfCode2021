import Data.List.Split (splitOn)

main :: IO Int
main = do
  file <- readFile "puzzle.txt"
  let ls = map parseLine (lines file)
      size = foldl1 (\(x0, y0) (x1, y1) -> (max x0 x1, max y0 y1)) (map maxXY ls)
      grid = replicate (fst size + 1) (replicate (snd size + 1) 0)
      points = concatMap linePoints ls
      filled = foldl (\grid (x, y) -> modify (modify (+ 1) y) x grid) grid points
  return (length . filter (> 1) . concat $ filled)

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
    else []

range :: Int -> Int -> [Int]
range a b = if a < b then [a .. b] else reverse [b .. a]

maxXY :: Line -> (Int, Int)
maxXY ((x0, y0), (x1, y1)) = (max x0 x1, max y0 y1)

modify :: (a -> a) -> Int -> [a] -> [a]
modify f i l =
  let (x, e : y) = splitAt i l
   in x ++ (f e : y)