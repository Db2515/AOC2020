import Data.List as L
import Data.List.Split
import Data.Map as M
import Data.Maybe
import System.IO

playGameTillTurn :: Int -> Map Int [Int] -> Int -> Int -> Int
playGameTillTurn endTurn prevTurns currTurn currNum
  | (succ endTurn) == currTurn = currNum
  | otherwise = if historyLookup == Nothing
                then error ("Unknown number: " ++ show currNum)
                else playGameTillTurn endTurn
                                      (M.insert age (currTurn:ageHistory) prevTurns)
                                      (succ currTurn)
                                      age
  where historyLookup = M.lookup currNum prevTurns
        Just turns = historyLookup
        age = if ((length turns) <= 1)
              then 0
              else (pred currTurn) - (head (tail turns))
        ageHistoryLookup = M.lookup age prevTurns
        ageHistory = if ageHistoryLookup == Nothing
                     then []
                     else fromJust ageHistoryLookup

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let formattedInput = L.map read (splitOn "," contents)
      startingTurn = (length formattedInput) + 1
      firstNum = (head (reverse formattedInput))::Int
      turnMap = fromList (zip formattedInput
                         (L.map (\x -> [x]) [1..]))
      result = playGameTillTurn 30000000 turnMap startingTurn firstNum
  print startingTurn
  print firstNum
  print turnMap
  print result
