import Data.List
import System.IO

countDifferencesInList :: [Int] -> (Int, Int, Int)
countDifferencesInList (x:[]) = (0,0,0)
countDifferencesInList (x:y:ns)
                  | (y-x) == 1 = ((succ a),b,c)
                  | (y-x) == 2 = (a,(succ b),c)
                  | (y-x) == 3 = (a,b,(succ c))
                  | otherwise = error ((show x) ++ "," ++ (show y))
                  where (a,b,c) = countDifferencesInList (y:ns)

countConsecutives :: [Int] -> Int
countConsecutives [] = 0
countConsecutives (a:[]) = 1
countConsecutives (a:b:ns) = if (b-a) == 1 
                             then succ (countConsecutives (b:ns))
                             else 1

listConsecutiveSizes :: [Int] -> [Int]
listConsecutiveSizes [] = [1]
listConsecutiveSizes ns = [consecutives] ++ 
                          listConsecutiveSizes (drop consecutives ns)
                        where consecutives = countConsecutives ns

mapConsecutiveSizesToChoices :: [Int] -> [Int]
mapConsecutiveSizesToChoices [] = []
mapConsecutiveSizesToChoices (x:xs)
                  | x == 1 = (1 : mapConsecutiveSizesToChoices xs)
                  | x == 2 = (1 : mapConsecutiveSizesToChoices xs)
                  | x == 3 = (2 : mapConsecutiveSizesToChoices xs)
                  | x == 4 = (4 : mapConsecutiveSizesToChoices xs)
                  | x == 5 = (7 : mapConsecutiveSizesToChoices xs)
                  | otherwise = error ("No mapping for" ++ show x)

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let formattedInput = [0] ++ (sort (map read (lines contents)))
      (oneDiff, twoDiff, threeDiff) = countDifferencesInList formattedInput
      result = product (mapConsecutiveSizesToChoices (listConsecutiveSizes formattedInput))
  print (show oneDiff ++ "," ++ show twoDiff ++ "," ++ show threeDiff)
  print result
