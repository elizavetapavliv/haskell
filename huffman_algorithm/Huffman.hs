module Huffman where

import qualified Data.Map as DM (Map, toList, fromListWith)
import Data.List (sortBy, insertBy)
import Data.Function (on)
import Data.ByteString.Internal (w2c)
import Data.Word (Word8)
import Data.List(unfoldr)

data HTree  = Leaf Char Int | Branch HTree HTree Int deriving (Show)
type CharWeight = (Char, Int) 
type Bits = [Bool]
type CodeMap = [(Char, Bits)]
type SwappedCodeMap = [(Bits, Char)]

encode:: String -> CodeMap -> Bits
encode input codeMap = concatMap (\sym -> maybe undefined id (lookup sym codeMap)) input

getWeightsList :: String -> [CharWeight]
getWeightsList input = DM.toList $ DM.fromListWith (+) [(sym, 1) | sym <- input]

buildCodeMap :: String -> CodeMap
buildCodeMap input = buildCodeMapHelper (buildTree (getWeightsList input)) []

buildCodeMapHelper :: HTree -> Bits -> CodeMap
buildCodeMapHelper (Leaf sym _) bs = [(sym, bs)]
buildCodeMapHelper (Branch left right _) bs = buildCodeMapHelper left (bs ++ [False]) ++ buildCodeMapHelper right (bs ++ [True])

buildTree :: [CharWeight] -> HTree
buildTree ts = buildFromTrees $ map (uncurry Leaf) (sortBy (compare `on` snd) ts) 

buildFromTrees :: [HTree] -> HTree
buildFromTrees [t] = t
buildFromTrees (t1: t2: ts) = buildFromTrees $ insertBy (compare `on` getWeight) (merge t1  t2) ts
    where merge t1 t2 = Branch t1 t2 (getWeight t1 + getWeight t2) 

getWeight:: HTree -> Int
getWeight (Leaf _ w) = w
getWeight (Branch _ _ w) = w

readCodeMap:: [Word8] -> SwappedCodeMap
readCodeMap bytes = readCodeMapHelper bytes []

readCodeMapHelper :: [Word8] -> SwappedCodeMap -> SwappedCodeMap
readCodeMapHelper [] codeMap = codeMap
readCodeMapHelper (sym:code:pad:bytes) codeMap = readCodeMapHelper bytes 
  (codeMap ++ [(take (length bits - (fromIntegral pad)) bits, w2c sym)]) 
    where bits = byteToBits code

decode :: Bits -> SwappedCodeMap -> String
decode bits codeMap = decodeHelper bits codeMap 0 ""

decodeHelper :: Bits -> SwappedCodeMap -> Int -> String -> String
decodeHelper bits codeMap ind decoded 
  | ind > length bits - 1 = decoded
  | otherwise = decodeHelper bits codeMap newInd (decoded ++ [bit])
      where (bit, newInd) = findBitSequence [bits !! ind] codeMap bits (ind + 1)

padLeft :: Bits -> Bits
padLeft bits = if len < 8 then ((replicate (8 - len) False) ++ bits) else bits
  where len = length bits

byteToBits :: Word8 -> Bits
byteToBits byte = padLeft $ reverse $ unfoldr (\byte -> if byte == 0 then Nothing 
  else Just(if rem byte 2 == 0 then False else True, div byte 2)) byte

findBitSequence :: Bits -> SwappedCodeMap -> Bits -> Int -> (Char, Int)
findBitSequence current codeMap bits ind = case lookup current codeMap of
        Just sym -> (sym, ind)
        Nothing -> findBitSequence (current ++ [bits !! ind]) codeMap bits (ind + 1)