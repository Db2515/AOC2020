import Data.List

getLowerHalf :: (Int, Int) -> (Int, Int)
getLowerHalf (lower, upper) = (lower, (div (upper - lower) 2) + lower)

getUpperHalf :: (Int, Int) -> (Int, Int)
getUpperHalf (lower, upper) = ((div (upper - lower) 2) + 1 + lower , upper)

findRowNumber :: [Char] -> (Int, Int) -> Int
findRowNumber [] (lower, upper) = if upper == lower then upper else -1
findRowNumber ('F':cs) bounds = findRowNumber cs (getLowerHalf bounds)
findRowNumber ('B':cs) bounds = findRowNumber cs (getUpperHalf bounds)
findRowNumber _ _ = -1

findColumnNumber :: [Char] -> (Int, Int) -> Int
findColumnNumber [] (lower, upper) = if upper == lower then upper else -1
findColumnNumber ('L':cs) bounds = findColumnNumber cs (getLowerHalf bounds)
findColumnNumber ('R':cs) bounds = findColumnNumber cs (getUpperHalf bounds)
findColumnNumber _ _ = -1 

getSeatPos :: [Char] -> (Int, Int)
getSeatPos seatAlloc = (findRowNumber (take 7 seatAlloc) (0, 127),
                        findColumnNumber (drop 7 seatAlloc) (0,7))

getSeatId :: [Char] -> Int
getSeatId seatAlloc = ((fst seatPos) * 8) + (snd seatPos)
                      where seatPos = getSeatPos seatAlloc

findSeat :: [Int] -> Int
findSeat [] = -1
findSeat (a:[]) = -1
findSeat (a:b:cs) = if (b - a) == 2 then a+1 else findSeat (b:cs)
