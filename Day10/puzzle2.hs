import Data.List (sort)

main = do
  file <- readFile "input.txt"
  let ls = map (++ [' ']) $ lines file
      ls' = filter (\x -> fst (checkChunk x) == 0) ls
  return (sort (map (fst . autoComplete) ls') !! (length ls' `div` 2))

checkChunk line =
  let starts = head line
   in if starts `elem` "<({["
        then
          let (inner_error, rest) = checkChunk (tail line)
              next = head rest
           in if inner_error > 0
                then (inner_error, " ")
                else
                  if next == ' '
                    then (0, " ")
                    else
                      if match starts next
                        then checkChunk (tail rest)
                        else (getErrorScore next, " ")
        else (0, line)

autoComplete line =
  let starts = head line
   in if starts `elem` "<({["
        then
          let (inner_completion, rest) = autoComplete (tail line)
              next = head rest
           in if next == ' '
                then (inner_completion * 5 + getCompletionScore starts, " ")
                else autoComplete (tail rest)
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

getCompletionScore '(' = 1
getCompletionScore '[' = 2
getCompletionScore '{' = 3
getCompletionScore '<' = 4
getCompletionScore _ = 999