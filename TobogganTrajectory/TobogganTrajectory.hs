import Data.List

countCollisions' :: [[Bool]] -> (Int, Int) -> Int -> Int -> Int
countCollisions' [] _ _ n = n
countCollisions' hill (x,y) i n = if (y >= (length hill)) then n
                                  else
                                    if (hill!!y)!!i
                                      then countCollisions' (hill') (x,y) i' (succ n)
                                      else countCollisions' (hill') (x,y) i' n
                                    where i' = (i+x) `mod` length(head hill)
                                          hill' = drop y hill

countCollisions :: [[Bool]] -> (Int ,Int) -> Int
countCollisions hill (x,y) = countCollisions' hill (x,y) x 0 

testRoutes :: [[Bool]] -> [(Int,Int)] -> [Int]
testRoutes hill coords = map (countCollisions hill) coords
