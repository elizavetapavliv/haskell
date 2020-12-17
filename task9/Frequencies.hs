module Frequencies where
import qualified Data.Map as DM (Map, toList, fromListWith)
import Data.List (sortBy, filter, elem)
import Data.Ord (comparing)

type FreqMap = [(String, Int)]

countFrequencies :: String -> FreqMap
countFrequencies text = sortBy (comparing snd) (countWordsFrequencies (extractWords text))

extractWords :: String -> [String]
extractWords text = words $ filter (not . (`elem` ".,!?:;(){}\"")) text

countWordsFrequencies :: [String] -> FreqMap
countWordsFrequencies words = DM.toList (DM.fromListWith (+) [(word, 1) | word <- words])