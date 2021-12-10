main = do
  file <- readFile "puzzle.txt"
  let ls = lines file
      ms = map parse ls
      pos = Pos {x = 0, depth = 0, aim = 0}
  return (foldl move pos ms)

data Pos = Pos {x :: Int, depth :: Int, aim :: Int} deriving (Show)

parse :: String -> (String, Int)
parse s = (head ws, read (last ws))
  where
    ws = words s

move :: Pos -> ([Char], Int) -> Pos
move Pos {x = x, depth = depth, aim = aim} ("forward", d) =
  Pos {x = x + d, depth = depth + aim * d, aim = aim}
move Pos {x = x, depth = depth, aim = aim} ("down", d) = Pos {x = x, depth = depth, aim = aim + d}
move Pos {x = x, depth = depth, aim = aim} ("up", d) = Pos {x = x, depth = depth, aim = aim - d}
move Pos {x = x, depth = depth, aim = aim} (_, d) = error "Invalid move"