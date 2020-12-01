import Data.List

getUniquePairs :: [a] -> [(a,a)]
getUniquePairs l = [(x,y) | (x:ys) <- tails l, y <- ys]
multOnTwoElemSum :: [Int] -> Int -> [Int] 
multOnTwoElemSum xs sum = [x*y | (x,y) <- getUniquePairs(xs), x+y == sum]

getUniqueTriples :: [a] -> [(a,a,a)]
getUniqueTriples l = [(x,y,z) | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs]
multOnThreeElemSum xs sum = [x*y*z | (x,y,z) <- getUniqueTriples(xs), 
                              x+y+z == sum]
