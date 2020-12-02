import Data.List

occurences :: Char -> [Char] -> Int
occurences y xs = length [ x | x <-xs, x == y] 

validSledShopPasswords :: [((Int, Int), Char, String)] -> [String]
validShedShopPasswords passwords = [ pass | ((min, max), y, pass) <- passwords, 
                                            occurences y pass >= min,
                                            occurences y pass <= max]
