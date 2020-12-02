import Data.List

occurences :: Char -> [Char] -> Int
occurences y xs = length [ x | x <-xs, x == y] 

validSledShopPasswords :: [((Int, Int), Char, String)] -> [String]
validSledShopPasswords passwords = [ pass | ((min, max), y, pass) <- passwords, 
                                            occurences y pass >= min,
                                            occurences y pass <= max]

validTobShopPasswords :: [((Int, Int), Char, String)] -> [String]
validTobShopPasswords passwords = [ pass | ((a, b), y, pass) <- passwords,
                                    (((pass !! (pred a)) == y) 
                                      && ((pass !! (pred b)) == y))
                                    || (((pass !! (pred b)) == y)
                                      && not ((pass !! (pred a)) == y))]
