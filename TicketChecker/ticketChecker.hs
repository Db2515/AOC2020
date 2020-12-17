import Data.List
import Data.List.Split
import System.IO

paragraphs :: [String] -> [[String]]
paragraphs = map (filter $ not . null) . groupBy (const $ not . null)

isInvalidField :: [((Int, Int), (Int, Int),String)] -> Int -> Bool
isInvalidField [] _ = True
isInvalidField (((ll,lu),(ul,uu),label):ranges) val
  | (ll <= val && val <= lu) || (ul <= val && val <= uu) = False
  | otherwise = isInvalidField ranges val

getInvalidFields :: [((Int, Int),(Int, Int),String)] -> [[Int]] -> [Int]
getInvalidFields ranges tickets = foldr1 (++) 
                                         (map (filter (isInvalidField ranges)) 
                                              tickets)

removeInvalidTickets :: [((Int, Int), (Int, Int), String)] -> [[Int]] -> [[Int]]
removeInvalidTickets ranges [] = []
removeInvalidTickets ranges (t:ts) = if foldr1 (||) (map (isInvalidField ranges)
                                                        t)
                                       then removeInvalidTickets ranges ts
                                       else (t:(removeInvalidTickets ranges ts))

isRangeValidForField :: [Int] -> ((Int, Int), (Int, Int), String) -> Bool
isRangeValidForField field range = foldr1 (&&) (map not ((map (isInvalidField [range])
                                                               field)))

deleteSingleRangeFields :: [((Int, Int), (Int, Int), String)]
                           -> [((Int, Int), (Int, Int), String)]
                           -> [((Int, Int), (Int, Int), String)]
deleteSingleRangeFields [] rangeFields = rangeFields
deleteSingleRangeFields (r:rs) rangeFields
  = deleteSingleRangeFields rs (delete r rangeFields)

removeSingleRangeFields :: [((Int, Int), (Int, Int), String)]
                           -> [[((Int, Int), (Int, Int), String)]] 
                           -> [[((Int, Int), (Int, Int), String)]] 
removeSingleRangeFields singleRangeFields fieldRanges
  = map (\x -> if ((length x) > 1)
               then (deleteSingleRangeFields singleRangeFields x)
               else x) fieldRanges



getSingleRangeFields :: [[((Int, Int), (Int, Int), String)]] 
                        -> [((Int, Int), (Int, Int), String)]
getSingleRangeFields [] = []
getSingleRangeFields (r:rs) = if (length r) == 1
                             then ((head r):(getSingleRangeFields rs))
                             else getSingleRangeFields rs


reduceFieldsToSingleRange :: [[((Int, Int), (Int, Int), String)]] 
                            -> [((Int, Int), (Int, Int), String)]
reduceFieldsToSingleRange fieldRanges = if (length singleRangeFields) 
                                              == (length fieldRanges)
                                        then map head fieldRanges
                                        else reduceFieldsToSingleRange
                                                (removeSingleRangeFields singleRangeFields
                                                                         fieldRanges)
  where singleRangeFields = getSingleRangeFields fieldRanges

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let formattedInput = (paragraphs . lines) contents
      ranges = map (\(label,rangeString) -> let ((ll:lu:[]):(ul:uu:[]):[]) = 
                                                    map (splitOn "-") rangeString
                                    in ((read ll, read lu),
                                        (read ul,read uu),
                                        label))
                   (map (\x -> let (label:ranges:[]) = (splitOn ":" x)
                               in (label,(splitOn " or " ranges)))
                        (formattedInput!!0))
      nearbyTickets = map (\x -> map read x) 
                          (map (splitOn ",") (tail ((formattedInput)!!2)))
      yourTicket = zip [1..] (map (\x -> map read x) 
                                  (map (splitOn ",") (tail ((formattedInput)!!1))))

      validTickets = removeInvalidTickets ranges nearbyTickets
      fields = transpose validTickets
      validRangesForFields = map (\field -> (filter (isRangeValidForField field) ranges))
                                 fields
  print (validRangesForFields)
  print (yourTicket::[(Int, [Int])])
  --print (zip [1..] (map (\(a,b,c) -> c) (reduceFieldsToSingleRange validRangesForFields)))
