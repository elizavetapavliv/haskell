import Frequencies
import System.IO
import System.Environment (getArgs)
import Control.Exception (bracket)

mapToString :: FreqMap -> String
mapToString [] = ""
mapToString ((word, freq):freqMap) = word ++ " " ++ show freq ++ "\n" ++ mapToString freqMap

main = do
  args <- getArgs
  if length args == 0 
    then putStr "Pass input file name as an argument"
    else do       
      let input = args!!0
      bracket
        (openFile input ReadMode)
        (hClose)
        (\handle ->  
           do
             contents <- hGetContents handle
             putStr $ mapToString $ countFrequencies contents
        )