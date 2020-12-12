import Data.Function
import Data.Ix
import Data.List
import System.IO

countOccurences :: (Eq a) => a -> [a]  -> Int
countOccurences elem = length . filter (==elem)

findFirstUpLeft :: [String] -> (Int, Int) -> Char
findFirstUpLeft room (y, x)
        | x < 0 || y < 0 = 'x'
        | x == 0 || y == 0 = seatValue
        | seatValue == 'L' || seatValue == '#' = seatValue
        | otherwise = findFirstUpLeft room (y-1,x-1)
        where seatValue = (room!!y)!!x

findFirstUp :: [String] -> (Int, Int) -> Char
findFirstUp room (y, x)
        | y < 0 = 'x'
        | y == 0 = seatValue
        | seatValue == 'L' || seatValue == '#' = seatValue
        | otherwise = findFirstUp room (y-1,x)
        where seatValue = (room!!y)!!x

findFirstUpRight :: [String] -> (Int, Int) -> Char
findFirstUpRight room (y, x)
        | y < 0 || x > ((length (room!!0)) - 1) = 'x'
        | y == 0 || x == ((length (room!!0)) - 1) = seatValue
        | seatValue == 'L' || seatValue == '#' = seatValue
        | otherwise = findFirstUpRight room (y-1,x+1)
        where seatValue = (room!!y)!!x

findFirstLeft :: [String] -> (Int, Int) -> Char
findFirstLeft room (y, x)
        | x < 0 = 'x'
        | x == 0 = seatValue
        | seatValue == 'L' || seatValue == '#' = seatValue
        | otherwise = findFirstLeft room (y,x-1)
        where seatValue = (room!!y)!!x

findFirstRight :: [String] -> (Int, Int) -> Char
findFirstRight room (y, x)
        | x > ((length (room!!0)) - 1) = 'x'
        | x == ((length (room!!0)) -1) = seatValue
        | seatValue == 'L' || seatValue == '#' = seatValue
        | otherwise = findFirstRight room (y,x+1)
        where seatValue = (room!!y)!!x

findFirstDownLeft :: [String] -> (Int, Int) -> Char
findFirstDownLeft room (y, x)
        | y > ((length room) - 1) || x < 0 = 'x'
        | y == ((length room) - 1) || x == 0 = seatValue
        | seatValue == 'L' || seatValue == '#' = seatValue
        | otherwise = findFirstDownLeft room (y+1,x-1)
        where seatValue = (room!!y)!!x

findFirstDown :: [String] -> (Int, Int) -> Char
findFirstDown room (y, x)
        | y > ((length room) - 1) = 'x'
        | y == ((length room) - 1) = seatValue
        | seatValue == 'L' || seatValue == '#' = seatValue
        | otherwise = findFirstDown room (y+1,x)
        where seatValue = (room!!y)!!x

findFirstDownRight :: [String] -> (Int, Int) -> Char
findFirstDownRight room (y, x)
        | y > ((length room) - 1) || x > ((length (room!!0)) - 1) = 'x'
        | y == ((length room) -1) || x == ((length (room!!0) -1)) = seatValue
        | seatValue == 'L' || seatValue == '#' = seatValue
        | otherwise = findFirstDownRight room (y+1,x+1) 
        where seatValue = (room!!y)!!x 

findVisableSeats :: [String] -> (Int, Int) -> String
findVisableSeats room (y,x) = [(findFirstUpLeft room (y-1,x-1)),
                                (findFirstUp room (y-1,x)),
                                (findFirstUpRight room (y-1,x+1)),
                                (findFirstLeft room (y,x-1)),
                                (findFirstRight room (y,x+1)),
                                (findFirstDownLeft room (y+1,x-1)),
                                (findFirstDown room (y+1,x)),
                                (findFirstDownRight room (y+1,x+1))]

findAdjacentNodes :: (Eq a) => [[a]] -> (Int, Int) -> [a]
findAdjacentNodes room (y,x) = (if y > 0
                                then
                                  if x > 0 
                                  then (take adjacentWidth (drop (x-1) (room!!(y-1))))
                                  else (take adjacentWidth (drop x (room!!(y-1))))
                                else [])
                                ++ (if x > 0 
                                    then (take 1 (drop (x-1) (room!!y)))
                                    else [])
                                ++ (if x < roomWidth
                                    then (take 1 (drop (x+1) (room!!y)))
                                    else [])
                                ++ (if y < roomLength
                                    then
                                      if x > 0
                                      then (take adjacentWidth (drop (x-1) (room!!(y+1))))
                                      else (take adjacentWidth (drop (x) (room!!(y+1))))
                                    else [])
                             where adjacentWidth = if x == 0 || x == roomWidth
                                                   then 2
                                                   else 3
                                   roomWidth = (length (room!!0) - 1)
                                   roomLength = (length room) - 1

findNextSeatValue :: [String] -> (Int, Int) -> Char
findNextSeatValue room (y,x)
  | (room!!y)!!x == 'L' && (countOccurences '#' adjacentNodes) == 0 = '#'
  | (room!!y)!!x == '#' && (countOccurences '#' adjacentNodes) >= 5 = 'L'
  | otherwise = (room!!y)!!x
  where adjacentNodes = findVisableSeats room (y,x)

findSeatingPlan :: [String] -> [String]
findSeatingPlan room = if room == newRoom
                       then newRoom
                       else findSeatingPlan newRoom
                where newRoom = map (map (\(y,x) -> findNextSeatValue room (y,x)))
                                    roomCoordinates
                      roomCoordinates = groupBy ((==) `on` fst) 
                                        (range ((0,0), ((length room) - 1,
                                                        (length (room!!0)) - 1)))

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let formattedInput = lines contents
  print (countOccurences '#' (foldr1 (++) (findSeatingPlan formattedInput)))
