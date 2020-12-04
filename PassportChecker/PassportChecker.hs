import Data.List

count :: [Bool] -> Int
count list = sum $ map fromEnum list

editNthElem :: Int -> a -> [a] -> [a]
editNthElem n value list = take n list ++ [value] ++ drop (succ n) list

checkHex :: String -> Bool
checkHex [] = True
checkHex (c:cs) = if (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
                then checkHex cs
                else False

checkPid :: String -> Bool
checkPid [] =  True
checkPid (c:cs) = if (c >= '0' && c <='9')
                then checkPid cs
                else False

checkFieldsExist' :: [(String, String)] -> [Int] -> Bool
checkFieldsExist' [] checks 
              | x == 8 = True
              | x == 7 = (checks!!1) == 0
              | otherwise = False
              where x = sum checks
checkFieldsExist' ((field, value):fs) checks
            | field == "byr" && value >= "1920" && value <= "2002" = 
                                  checkFieldsExist' fs (editNthElem 0 1 checks)
            | field == "cid" = 
                                  checkFieldsExist' fs (editNthElem 1 1 checks)
            | field == "ecl" && (value == "amb" || value == "blu" 
                                  || value == "brn" || value == "gry"
                                  || value == "grn" || value == "hzl"
                                  || value == "oth") = 
                                  checkFieldsExist' fs (editNthElem 2 1 checks)
            | field == "eyr" && value >= "2020" && value <= "2030" =
                                  checkFieldsExist' fs (editNthElem 3 1 checks)
            | field == "hcl" && (head value) == '#' && (length value) == 7 &&
              checkHex (tail value) = 
                                  checkFieldsExist' fs (editNthElem 4 1 checks)
            | field == "hgt" && (( unit == "cm" && val >= "150" && val <= "193") ||
                                  ( unit == "in" && val >= "59" && val <= "76")) = 
                                  checkFieldsExist' fs (editNthElem 5 1 checks)
            | field == "iyr" && value >= "2010" && value <= "2020" =
                                  checkFieldsExist' fs (editNthElem 6 1 checks)
            | field == "pid" && (length value) == 9 && checkPid value = 
                                  checkFieldsExist' fs (editNthElem 7 1 checks)
            | otherwise = checkFieldsExist' fs checks
            where unit = drop ((length value) - 2) value
                  val = take ((length value) - 2) value

checkFieldsExist :: [(String, String)] -> Bool
checkFieldsExist fields = checkFieldsExist' fields
                                            (take 8 (repeat 0))
