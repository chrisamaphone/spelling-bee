import Data.Maybe
import Data.List

-- main = interact atLeastSevenLetters
main = interact beeLines

matchLines :: (String -> Bool) -> String -> String
matchLines f input =
  let 
    allLines = lines input
    matches = filter f allLines
  in
    unlines matches

atLeastSevenLetters = matchLines (\line -> length line > 6)
exactlySevenLetters = matchLines (\line -> length line == 7)

addNoDupSorted :: Ord a => [a] -> [a] -> a -> [a]
addNoDupSorted pre [] x = pre++[x]
addNoDupSorted pre (y:rest) x = if y == x then pre++(y:rest)
  else if y < x then addNoDupSorted (pre++[y]) rest x
  else pre ++ (x:y:rest)

addNoDup :: Eq a => [a] -> a -> [a]
addNoDup l x = if (elem x l) then l else x:l

removeDups :: String -> String
removeDups s = foldl (\accum -> \new -> addNoDup accum new) [] s

removeDupsSorted :: String -> String
removeDupsSorted s 
  = foldl (\acc -> \new -> addNoDupSorted [] acc new) [] s

isBeeWord :: String -> Bool
isBeeWord w =
  (length w > 6) && (not (elem 's' w)) && (length (removeDups w) == 7)

wordAndLetters w =
  w ++ ":   " ++ (removeDupsSorted w)

formatPair (s1, s2) =
  s1 ++ ":   " ++ s2

beeWord :: String -> Maybe (String, String) 
beeWord w =
  if length w > 6 && not (elem 's' w)
  then
    let dedup = removeDupsSorted w
    in
      if length dedup == 7 then
        Just (w, dedup)
      else
        Nothing
  else
    Nothing

sortBySecond = sortBy (\(_,a) (_,b) -> compare a b)
    
-- beeLines = matchLines beeWord 

-- beeLines input = output, where input and output are multi-line file
-- contents, one word per line
beeLines :: String -> String
beeLines input =
  let 
    allLines = lines input
    -- matches = filter beeWord allLines
    -- formatted = map wordAndLetters matches
    matches = mapMaybe beeWord allLines
    sorted = sortBySecond matches
  in
    unlines (map formatPair sorted)
