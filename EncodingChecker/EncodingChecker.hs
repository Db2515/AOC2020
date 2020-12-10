import Data.List
import System.IO

getUniquePairs :: [a] -> [(a,a)]
getUniquePairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

sumInList :: Int -> [Int] -> Bool
sumInList sum xs = foldr (||) False [True | (x,y) <- getUniquePairs xs, 
                                            x+y == sum]

findValueNotSumInPrevN :: Int -> [Int] -> Int
findValueNotSumInPrevN n xs
                | (length xs) == n = -1
                | sumInList (xs!!n) (take n xs) = findValueNotSumInPrevN n 
                                                                        (tail xs)
                | otherwise = xs!!n

findMaxMinThatSumFromRange :: Int -> [[Int]] -> Int
findMaxMinThatSumFromRange n (x:xs) = if sum x == n && not (n `elem` x)
                                      then (maximum x) + (minimum x)
                                      else findMaxMinThatSumFromRange n xs

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let formattedInput = map read (lines contents)
      badValue = findValueNotSumInPrevN 25 formattedInput
      allSublists = nub (foldr1 (++) (map (tails . reverse) (tails (formattedInput))))
      result =  findMaxMinThatSumFromRange badValue allSublists
  print badValue
  print result
