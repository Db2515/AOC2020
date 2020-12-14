import Control.Monad
import Data.Bits
import Data.Char
import Data.List as L
import Data.List.Split
import Data.Map as M
import System.IO

toDec :: String -> Int
toDec = L.foldl' (\acc x -> acc * 2 + digitToInt x) 0

genAndMask :: String -> String
genAndMask [] = []
genAndMask ('X':bits) = ('1':(genAndMask bits))
genAndMask (bit:bits) = (bit:(genAndMask bits))

genOrMask :: String -> String
genOrMask [] = []
genOrMask ('X':bits) = ('0':(genOrMask bits))
genOrMask (bit:bits) = (bit:(genOrMask bits))

splitMask :: String -> (Int, Int)
splitMask mask = ((toDec (genOrMask mask)), (toDec (genAndMask mask)))

getFirstNum :: String -> String
getFirstNum [] = []
getFirstNum (c:cs) = if isDigit c
                     then (c:getFirstNum cs)
                     else []

extractNumbers :: String -> (Int, Int)
extractNumbers x = (read (getFirstNum firstHalf), read (getFirstNum secondHalf))
  where (firstHalf:secondHalf:[]) = splitOn " = " x

runProgram' :: Map Int Int -> (Int, Int) -> [String] -> Map Int Int
runProgram' memory _ [] = memory
runProgram' memory _ (('m':'a':'s':'k':' ':'=':' ':mask):is) =
  runProgram' memory (splitMask mask) is
runProgram' memory (orMask, andMask) (('m':'e':'m':'[':remaining):is) =
  runProgram' (M.insert n ((.&.) andMask ((.|.) orMask val)) memory)
              (orMask, andMask)
              is
              where (n, val) = extractNumbers remaining
runProgram' _ _ (i:is) = error ((show i) ++ " is not a valid instruction")

runProgram :: [String] -> Map Int Int
runProgram = runProgram' (fromList []) (0, minBound :: Int)

count :: Char -> String -> Int
count c cs = (length . L.filter (== c)) cs

applyPermutation :: String -> String -> String
applyPermutation mask [] = mask
applyPermutation ('X':bits) (x:xs) = (x:(applyPermutation bits xs))
applyPermutation (bit:bits) subs = ('1':(applyPermutation bits subs))

genXOrMask :: String -> String -> String
genXOrMask [] _ = []
genXOrMask (x:xs) (y:ys) = if x == 'X'
                           then (y:(genXOrMask xs ys))
                           else ('0':(genXOrMask xs ys))

genXAndMask :: String -> String -> String
genXAndMask [] _ = []
genXAndMask (x:xs) (y:ys) = if x == 'X'
                            then (y:(genXAndMask xs ys))
                            else ('1':(genXAndMask xs ys))

genXMask :: String -> String -> (Int, Int)
genXMask mask perm = (toDec (genXOrMask mask perm), toDec (genXAndMask mask perm))

getPossibleMasks :: String -> [(Int,Int)]
getPossibleMasks mask = L.map (genXMask mask) (L.map (applyPermutation mask) 
                                      (replicateM (count 'X' mask) "01"))

-- Mask needs to be split into xOrMask xAndMask and orMask
runProgram2' :: Map Int Int -> [(Int,Int)] -> Int -> [String] -> Map Int Int
runProgram2' memory _  _ [] = memory
runProgram2' memory _ _ (('m':'a':'s':'k':' ':'=':' ':mask):is) =
  runProgram2' memory (getPossibleMasks mask) (toDec (genOrMask mask)) is
runProgram2' memory masks  orMask (('m':'e':'m':'[':remaining):is) =
  runProgram2' (M.union (fromList (L.map (\(xOrMask, xAndMask) -> 
                                            (((.|.) xOrMask ((.&.) xAndMask ((.|.) orMask n))), 
                                                val)) 
                                          masks)) 
                        memory)
              masks
              orMask
              is
              where (n, val) = extractNumbers remaining
runProgram2' _ _ _ (i:is) = error ((show i) ++ " is not a valid instruction")


runProgram2 :: [String] -> Map Int Int
runProgram2 = runProgram2' (fromList []) [] 0 

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let formattedInput = lines contents
      memory = runProgram2 formattedInput
      result = sum (L.map snd (M.toList memory)) 
  print memory
  print result
