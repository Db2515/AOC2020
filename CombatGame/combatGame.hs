import Data.List
import System.IO

paragraphs :: [String] -> [[String]]
paragraphs = map (filter $ not . null) . groupBy (const $ not . null)

addMultiplyWithReverseIndex' :: Int -> [Int] -> Int
addMultiplyWithReverseIndex' _ [] = 0
addMultiplyWithReverseIndex' i (x:xs) = i*x + (addMultiplyWithReverseIndex' (pred i)
                                                                            xs)

addMultiplyWithReverseIndex :: [Int] -> Int
addMultiplyWithReverseIndex xs = addMultiplyWithReverseIndex' (length xs) xs

combat :: ([Int], [Int]) -> (Int, [Int])
combat ([], p2) = (2, p2)
combat (p1, []) = (1, p1)
combat ((c1:c1s),(c2:c2s))
  | c1 > c2 = combat ((c1s ++ [c1,c2]),c2s)
  | c1 < c2 = combat (c1s, (c2s ++ [c2,c1]))
  | otherwise = combat ((c1s ++ [c1]), (c2s ++ [c2]))

recursiveCombat' :: ([[Int]],[[Int]]) -> ([Int], [Int]) -> (Int, [Int])
recursiveCombat' _ ([], p2) = (2, p2)
recursiveCombat' _ (p1, []) = (1, p1)
recursiveCombat' (p1His, p2His) (p1,p2)
  | p1 `elem` p1His && p2 `elem` p2His = (1, p1)
  | c1 <= (length c1s) && c2 <= (length c2s) = if subWinner == 1
                                               then recursiveCombat' newHistory
                                                                     ((c1s ++ [c1,c2]),
                                                                      c2s)
                                               else recursiveCombat' newHistory
                                                                     (c1s,
                                                                      (c2s ++ [c2,c1]))
  | c1 > c2 = recursiveCombat' newHistory ((c1s ++ [c1,c2]),c2s)
  | c1 < c2 = recursiveCombat' newHistory (c1s, (c2s ++ [c2,c1]))
  | otherwise = recursiveCombat' newHistory ((c1s ++ [c1]), (c2s ++ [c2]))
  where (c1:c1s) = p1
        (c2:c2s) = p2
        (subWinner, _) = recursiveCombat' ([],[]) ((take c1 c1s), (take c2 c2s))
        newHistory = ((p1:p1His), (p2:p2His))

recursiveCombat :: ([Int], [Int]) -> (Int, [Int])
recursiveCombat (p1,p2) = recursiveCombat' ([],[]) (p1,p2)

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let (p1:p2:[]) = map (\x -> map read (tail x)) 
                           ((paragraphs . lines) contents)
  print (addMultiplyWithReverseIndex(snd (combat (p1,p2))))
  print (recursiveCombat (p1,p2))
  print (addMultiplyWithReverseIndex(snd (recursiveCombat (p1,p2))))
