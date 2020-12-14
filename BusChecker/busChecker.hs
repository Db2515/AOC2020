import Data.List
import Data.List.Split
import System.IO

minWaitTimeTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
minWaitTimeTuple (idA, timeA) (idB, timeB) = if timeA <= timeB
                                             then (idA, timeA)
                                             else (idB, timeB)

findSmallestWaitTimeBusId :: Int -> [Int] -> (Int, Int)
findSmallestWaitTimeBusId arriveTime (id:[]) = (id, (id - (arriveTime `mod` id)))
findSmallestWaitTimeBusId arriveTime (id:ids) = minWaitTimeTuple (id, waitTime)
                                                                 (findSmallestWaitTimeBusId arriveTime
                                                                                           ids)
                                                where waitTime = id - (arriveTime `mod` id)

isSequentialBusTime :: Int -> [(Int, Int)] -> Bool
isSequentialBusTime _ [] = True
isSequentialBusTime time ((pos, id):ids) = if ((time + pos) `mod` id) == 0
                                           then isSequentialBusTime time ids
                                           else False

findSequentialBusTime :: Int -> [(Int, Int)] -> Int
findSequentialBusTime time ids = if isSequentialBusTime time ids
                                 then time
                                 else findSequentialBusTime (time + (snd (ids!!0)))
                                                            ids

findFirstDiv :: Int -> Int -> Int
findFirstDiv n divisor = if (n `mod` divisor) == 0
                         then n
                         else findFirstDiv (n+1) divisor

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let formattedInput = lines contents
      arriveTime = (read (formattedInput!!0))::Int
      busIDs = map read ( filter (/="") (splitOn "," (filter (/='x') (formattedInput!!1))))
      (id, time) = findSmallestWaitTimeBusId arriveTime busIDs
      numberedBusIDs = (map (\(x,y) -> (x, read y)) (filter (\(x,y) -> y/="x") 
                          (zip [0..] (splitOn "," (formattedInput!!1)))))::[(Int,Int)]
      firstDiv = findFirstDiv 100000000000000 (snd (numberedBusIDs!!0))
      seqTime = findSequentialBusTime firstDiv numberedBusIDs
  print arriveTime
  print busIDs
  print (id * time)
  print numberedBusIDs
  print firstDiv
  print seqTime
