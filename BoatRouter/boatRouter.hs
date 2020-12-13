import Data.List
import System.IO

sumTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTuples (a,b) (c,d) = (a+c, b+d)

calcPosChange :: Int -> Int -> (Int, Int)
calcPosChange heading distance
  | heading == 0 = (distance, 0)
  | heading == 90 = (0, distance)
  | heading == 180 = ((negate distance), 0)
  | heading == 270 = (0, (negate distance))
  | otherwise = error "Invalid heading"

completeInstrucs' :: Int -> [(Char, Int)] -> (Int, Int)
completeInstrucs' _ [] = (0,0)
completeInstrucs' heading ((op,arg):instrucs)
  | op == 'N' = sumTuples (arg, 0) (completeInstrucs' heading instrucs)
  | op == 'S' = sumTuples ((negate arg),0) (completeInstrucs' heading instrucs) 
  | op == 'E' = sumTuples (0, arg) (completeInstrucs' heading instrucs)
  | op == 'W' = sumTuples (0,(negate arg)) (completeInstrucs' heading instrucs)
  | op == 'R' = completeInstrucs' ((heading + arg) `mod` 360) instrucs
  | op == 'L' = completeInstrucs' ((360 + (heading - arg)) `mod` 360)
                                    instrucs
  | op == 'F' = sumTuples (calcPosChange heading arg) 
                          (completeInstrucs' heading instrucs)
  | otherwise = error ([op] ++ " is an invalid operation")

completeInstrucs :: [(Char, Int)] -> (Int, Int)
completeInstrucs = completeInstrucs' 90

rotateRight ::  (Int, Int) -> Int -> (Int, Int)
rotateRight (x,y) degrees
  | (degrees `mod` 360) == 0 = (x,y)
  | (degrees `mod` 360) == 90 = (negate y, x)
  | (degrees `mod` 360) == 180 = (negate x, negate y)
  | (degrees `mod` 360) == 270 = (y, negate x)
  | otherwise = error "Invalid rotation"

rotateLeft :: (Int, Int) -> Int -> (Int, Int)
rotateLeft (x,y) degrees
  | (degrees `mod` 360) == 0 = (x,y)
  | (degrees `mod` 360) == 90 = (y, negate x)
  | (degrees `mod` 360) == 180 = (negate x, negate y)
  | (degrees `mod` 360) == 270 = (negate y, x)
  | otherwise = error "Invalid rotation"

completeInstrucsWithWaypoint' :: (Int, Int) -> [(Char, Int)] -> (Int, Int)
completeInstrucsWithWaypoint' _ [] = (0, 0)
completeInstrucsWithWaypoint' waypoint ((op, arg):instrucs)
  | op == 'N' = completeInstrucsWithWaypoint' (sumTuples (arg,0) waypoint)
                                              instrucs
  | op == 'S' = completeInstrucsWithWaypoint' (sumTuples ((negate arg),0) waypoint)
                                              instrucs
  | op == 'E' = completeInstrucsWithWaypoint' (sumTuples (0, arg) waypoint)
                                              instrucs
  | op == 'W' = completeInstrucsWithWaypoint' (sumTuples (0,(negate arg)) waypoint)
                                              instrucs
  | op == 'R' = completeInstrucsWithWaypoint' (rotateRight waypoint arg) instrucs
  | op == 'L' = completeInstrucsWithWaypoint' (rotateLeft waypoint arg) instrucs
  | op == 'F' = let (x,y) = waypoint in sumTuples (x*arg, y*arg) (completeInstrucsWithWaypoint' waypoint instrucs) 
completeInstrucsWithWaypoint :: [(Char, Int)] -> (Int, Int)
completeInstrucsWithWaypoint = completeInstrucsWithWaypoint' (1, 10)

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let formattedInput = map (\(op:arg) -> (op, (read arg)::Int)) (lines contents)
      (x,y) = completeInstrucsWithWaypoint formattedInput
      manhattenDistance = (abs x) + (abs y)
      substeps = (map (reverse) . reverse .tails . reverse) formattedInput
  print (map (completeInstrucsWithWaypoint) substeps)
  print manhattenDistance
