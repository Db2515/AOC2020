import Data.List as L
import Data.List.Split
import Data.Map as M
import System.IO

type Address = Int
type Instr = (String, Int)
type InstrMem = Map Address Instr

calcAccBeforeLoop' :: Int -> Int -> [Int] -> InstrMem -> Int
calcAccBeforeLoop' pc acc execInstrs iMem
            | lookupResult == Nothing = error "Jumped outside the program"
            | pc `elem` execInstrs = acc
            | op == "nop" = calcAccBeforeLoop' (succ pc) acc 
                                               (pc:execInstrs) iMem
            | op == "acc" = calcAccBeforeLoop' (succ pc) (acc + arg)
                                               (pc:execInstrs) iMem
            | op == "jmp" = calcAccBeforeLoop' (pc + arg) acc
                                               (pc:execInstrs) iMem
            | otherwise = error "Unexpected instruction"
            where lookupResult = M.lookup pc iMem
                  Just (op, arg) = lookupResult

calcAccBeforeLoop :: InstrMem -> Int
calcAccBeforeLoop = calcAccBeforeLoop' 0 0 []

calcAccProgTerm :: Int -> Int -> [Int] -> InstrMem -> (Int, Bool)
calcAccProgTerm pc acc execInstrs iMem
            | lookupResult == Nothing = error "Jumped outside the program"
            | op == "end" = (acc, True)
            | pc `elem` execInstrs = (acc, False)
            | op == "nop" = calcAccProgTerm (succ pc)
                                            acc 
                                            (pc:execInstrs)
                                            iMem
            | op == "acc" = calcAccProgTerm (succ pc)
                                            (acc + arg)
                                            (pc:execInstrs)
                                            iMem
            | op == "jmp" = calcAccProgTerm (pc + arg)
                                            acc
                                            (pc:execInstrs)
                                            iMem
            | otherwise = error "Unexpected instruction"
            where lookupResult = M.lookup pc iMem
                  Just (op, arg) = lookupResult

maxTermProg :: ((Int,Bool),(Int,Bool)) -> (Int, Bool)
maxTermProg (x,y) = (max (if (snd x) then (fst x) else (-1))
                        (if (snd y) then (fst y) else (-1)),
                     snd x || snd y)

-- For nop and jmp ops take max of with and without swap using calcAccBeforeLoop
-- to calc remaining instructions
calcAccBeforeLoopWithSwap' :: Int -> Int -> [Int] -> InstrMem -> (Int, Bool)
calcAccBeforeLoopWithSwap' pc acc execInstrs iMem
            | lookupResult == Nothing = error "Jumped outside the program"
            | pc `elem` execInstrs = (acc, False)
            | op == "nop" = maxTermProg ((calcAccBeforeLoopWithSwap' (succ pc)
                                                       acc
                                                       (pc:execInstrs)
                                                       iMem),
                                     (calcAccProgTerm pc
                                                      acc
                                                      execInstrs
                                                      (M.insert pc ("jmp", arg) iMem)))
            | op == "acc" = calcAccBeforeLoopWithSwap' (succ pc)
                                                       (acc + arg)
                                                       (pc:execInstrs)
                                                       iMem
            | op == "jmp" = maxTermProg ((calcAccBeforeLoopWithSwap' (pc + arg)
                                                       acc
                                                       (pc:execInstrs)
                                                       iMem),
                                     (calcAccProgTerm pc
                                                      acc
                                                      execInstrs
                                                      (M.insert pc ("nop", arg) iMem)))
            | otherwise = error "Unexpected instruction"
            where lookupResult = M.lookup pc iMem
                  Just (op, arg) = lookupResult


calcAccBeforeLoopWithSwap :: InstrMem -> Int
calcAccBeforeLoopWithSwap = fst . calcAccBeforeLoopWithSwap' 0 0 []

formatInput :: String -> Instr
formatInput line = (op,arg)
            where (op:(sign:num):[]) = splitOn " " line
                  arg = if sign == '-'
                        then (-1) * (read num)
                        else read num

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let formattedInput = L.zip [0..] (L.map formatInput (lines contents))
      iMem = M.fromList formattedInput
  print iMem
  print (calcAccBeforeLoopWithSwap iMem)
