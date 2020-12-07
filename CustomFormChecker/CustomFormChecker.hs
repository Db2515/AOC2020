import Data.List
import System.IO

countYesFromEveryone :: [String] -> Int
countYesFromEveryone answers = (length . 
                                filter (\x -> (length x) == (length answers)) .
                                group . sort . concat) answers

uniq :: Ord b => [b] -> [b]
uniq = map head . group . sort

countYesFromAnyone :: [String] -> Int
countYesFromAnyone = length . filter (\c -> c >= 'a' && c <= 'z') . uniq . concat

paragraphs :: [String] -> [[String]]
paragraphs = map (filter $ not . null) . groupBy (const $ not . null)

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let formattedInput = (paragraphs . lines) contents
      groupValues = map countYesFromEveryone formattedInput
  print (sum groupValues)
  hClose handle
