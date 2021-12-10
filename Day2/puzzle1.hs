main :: IO Pos
main = do
  file <- readFile "puzzle.txt"
  let ls = lines file
      ms = map parse ls
      pos = Pos {x = 0, depth = 0}
  return (foldl move pos ms)

data Pos = Pos {x :: Int, depth :: Int} deriving (Show)

parse :: String -> (String, Int)
parse s = (head ws, read (last ws))
  where
    ws = words s

move :: Pos -> ([Char], Int) -> Pos
move Pos {x = x, depth = depth} ("forward", d) = Pos {x = x + d, depth = depth}
move Pos {x = x, depth = depth} ("down", d) = Pos {x = x, depth = depth + d}
move Pos {x = x, depth = depth} ("up", d) = Pos {x = x, depth = depth - d}
move Pos {x = x, depth = depth} (_, d) = error "Invalid move"