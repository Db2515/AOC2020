import Data.List as L
import Data.List.Split
import Data.Map as M
import System.IO

yourColorInOuterColor :: Map String [String] -> String -> String -> Bool
yourColorInOuterColor rules yourColor outerColor 
                      = if lookupResult == Nothing || lookupResult == (Just []) 
                        then False
                        else if yourColor `elem` innerBags
                             then True
                             else True `elem` (L.map (yourColorInOuterColor rules yourColor) innerBags)
                        where lookupResult = M.lookup outerColor rules
                              Just innerBags = lookupResult

findOuterBagColors :: [String] -> Map String [String] -> String -> [String]
findOuterBagColors outerColors rules yourColor
                    = [color | color <- outerColors, 
                          yourColorInOuterColor rules yourColor color]

formatJustInnerBags :: String -> [String]
formatJustInnerBags "noother" = []
formatJustInnerBags innerBags = L.map (\(c:cs) -> cs) 
                                (splitOn "," innerBags)

simpleFormatInput :: String -> (String, [String])
simpleFormatInput input = (outerBag, formatJustInnerBags innerBags) 
            where (outerBag:innerBags:[]) = (splitOn "contain" 
                                              . remove " "
                                              . remove "."
                                              . remove "bag"
                                              . remove "bags") input

numInnerBags :: Map String [(String, Int)] -> String -> Int
numInnerBags rules outerColor 
              = if lookupResult == Nothing then -1
                else if lookupResult == Just []
                     then 0
                     else sum (L.map (\(x,y) -> y + y*(numInnerBags rules x)) 
                                pairings)
                where lookupResult = M.lookup outerColor rules
                      Just pairings = lookupResult

                                

formatInnerBags :: String -> [(String, Int)]
formatInnerBags "noother" = []
formatInnerBags innerBags = L.map (\(c:cs) -> (cs, read [c])) 
                                (splitOn "," innerBags)


formatInput :: String -> (String, [(String, Int)])
formatInput input = (outerBag, formatInnerBags innerBags) 
            where (outerBag:innerBags:[]) = (splitOn "contain" 
                                              . remove " "
                                              . remove "."
                                              . remove "bag"
                                              . remove "bags") input
remove :: String -> String -> String
remove w "" = ""
remove w s@(c:cs) 
  | w `isPrefixOf` s = remove w (L.drop (length w) s)
  | otherwise = c : remove w cs

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let simpleFormattedInput = L.map simpleFormatInput (lines contents)
      simpleRules = M.fromList simpleFormattedInput
      outerColors = L.map fst simpleFormattedInput
      formattedInput = L.map formatInput (lines contents)
      rules = M.fromList formattedInput
  print (length (findOuterBagColors outerColors simpleRules "shinygold"))
  print (numInnerBags rules "shinygold")
