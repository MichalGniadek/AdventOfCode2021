import Data.List (group, sort)
import Data.List.Split (splitOn)

main = do
  file <- readFile "input.txt"
  let ls = lines file
      polym = head ls
      prods = map (splitOn " -> ") $ drop 2 ls
      final = applyProd polym prods 40
      c = sort . map length . group $ sort final
  print (last c - head c)

applyProd polym prods 0 = polym
applyProd polym prods n =
  let pairs = zip polym (tail polym)
      newPairs = map (applyToPair prods) pairs
      newPolym = head (head newPairs) : concatMap tail newPairs
   in applyProd newPolym prods (n -1)

applyToPair :: [[[Char]]] -> (Char, Char) -> [Char]
applyToPair prods pair =
  case filter (\[[a, b], _] -> a == fst pair && b == snd pair) prods of
    [] -> [fst pair, snd pair]
    [[l, r]] -> [fst pair, head r, snd pair]
    x -> error ("nope" ++ show x ++ "asd")