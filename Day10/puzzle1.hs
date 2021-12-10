main = do
  file <- readFile "input.txt"
  let ls = map (++ [' ']) $ lines file
  return (sum $ map (fst . parseChunk) ls)

parseChunk line =
  let starts = head line
   in if starts `elem` "<({["
        then
          let (inner_error, rest) = parseChunk (tail line)
              next = head rest
           in if inner_error > 0
                then (inner_error, " ")
                else
                  if next == ' '
                    then (0, " ")
                    else
                      if match starts next
                        then parseChunk (tail rest)
                        else (getErrorScore next, " ")
        else (0, line)

match '(' ')' = True
match '{' '}' = True
match '[' ']' = True
match '<' '>' = True
match _ _ = False

getErrorScore ')' = 3
getErrorScore ']' = 57
getErrorScore '}' = 1197
getErrorScore '>' = 25137
getErrorScore _ = 999