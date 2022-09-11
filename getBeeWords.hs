import Data.Maybe
import Data.List
import qualified Data.Map as Map

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

-- sort a list of pairs by their second elements
-- sortBySecond = sortBy (\(_,a) (_,b) -> compare a b)
-- unnecessary if we use the Data.Map structure, b/c insertion sorts.

swap (a,b) = (b,a)

-- redone in points free style
-- collate list of (k, v) w/duplicate ks into (k, [vs]) map
-- collate kvpairs =
--   let
--     singles = map (\(x,y) -> (x,[y])) kvpairs
--   in
--     Map.fromListWith (++) singles

-- build a map whose keys are bees and values are lists of pangrams
collate = Map.fromListWith (++) . map (\(x,y) -> (x,[y]))

-- pangramMap :: [(pangram, bee)] -> map from bee to all pangrams
pangramMap = collate . map swap

-- turn into (bee, "pangram1, pangram2, etc"]) pairs
mapToBeePangramsList beeWordsMap =
  let
    beeWordsList = Map.toList beeWordsMap    
  in
    map (\(key, val) -> (key, intercalate ", " val)) beeWordsList
    

-- Top level function
-- beeLines input = output, where input and output are multi-line file
-- contents, one word per line
beeLines :: String -> String
beeLines input =
  let 
    allLines = lines input
    -- matches = filter beeWord allLines
    -- formatted = map wordAndLetters matches
    matches = mapMaybe beeWord allLines
    pangrams = pangramMap matches -- n.b.: hook in here to do more
    pangramsList = mapToBeePangramsList pangrams
    -- sorted = sortBySecond matches
  in  -- turn back into a blob of text
    unlines (map formatPair pangramsList)


main = interact beeLines

