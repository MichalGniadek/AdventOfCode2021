import Data.List (findIndex, groupBy, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

main = do
  file <- readFile "input.txt"
  let ls = lines file
      polym = head ls
      pairs = zip polym (tail polym)
      prods = map ((\x -> (0, (head $ head x, last $ head x), last $ last x, (-1, -1))) . splitOn " -> ") $ drop 2 ls
      empty = map (\(_, patt, to, _) -> (0, patt, to, findIndexes patt to prods)) prods
      prods' = map (\(_, patt, to, nxt) -> (length $ filter (== patt) pairs, patt, to, nxt)) empty
      fin_pairs = runN prods' empty 40
      fin = [(1, head polym), (1, last polym)] ++ concatMap (\(count, (p0, p1), _, _) -> [(count, p0), (count, p1)]) fin_pairs
      fin' = sortOn snd fin
      fin'' = groupBy (\x y -> snd x == snd y) fin'
      fin''' = sort $ map (sum . map fst) fin''
  print ((last fin''' - head fin''') `div` 2)

runN prods new 0 = prods
runN prods new n = runN (run prods new) new (n -1)

run [] new = new
run prods new =
  let (count, _, _, (a, b)) = head prods
   in run (tail prods) (add count (add count new a) b)

add a prods Nothing = prods
add a prods (Just i) =
  map
    ( \(ind, p@(count, patt, to, nxt)) ->
        if ind == i
          then (count + a, patt, to, nxt)
          else p
    )
    (enumarate prods)

enumarate = zip [0 ..]

findIndexes patt to prods =
  let a = (fst patt, to)
      b = (to, snd patt)
   in ( findIndex (\(_, patt, _, _) -> patt == a) prods,
        findIndex (\(_, patt, _, _) -> patt == b) prods
      )