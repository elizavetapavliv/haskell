import Huffman

import System.IO
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC8
import Data.ByteString.Internal (c2w)
import Control.Exception (bracket)
import Data.Binary.Put (putWord8, runPut, Put)
import Data.Binary.Get (Get, runGet, getWord8, getLazyByteString, getRemainingLazyByteString)
import Data.Word (Word8)

main = do
    args <- getArgs
    case length args of
        0 -> putStr "Pass input and output file names as arguments"
        1 -> putStr "Pass output file name as second argument"
        2 -> putStr "Pass mode (e - encode, d - decode) as third argument"
        otherwise -> do       
            let input = args!!0
            let output = args!!1
            let mode = args!!2
            bracket
              (openBinaryFile input ReadMode)
              (hClose)
              (\handle ->  
                 case mode of
                   "e" -> processEncoding handle output
                   "d" -> processDecoding handle output
                   otherwise -> putStr "Usage: e - encode, d - decode"
              )

splitBits :: Bits -> [Bits]
splitBits [] = []
splitBits bits = x : splitBits y
  where (x,y) = splitAt 8 bits
  
bitsToByte :: Bits -> Word8  
bitsToByte bits = foldl (\byte bit -> byte*2 + if bit then 1 else 0) 0 bits

packBits :: Bits -> ([Word8], Int) 
packBits bits = (map bitsToByte (splitBits paddedBits), pad)
  where (paddedBits, pad) = padRight bits

padRight :: Bits -> (Bits, Int)
padRight bits = if pad /= 0 then (bits ++ (replicate (8 - pad) False), 8 - pad) else (bits, 0)
  where pad = rem (length bits) 8
  
mapToBytes:: CodeMap -> [Word8]
mapToBytes [] = []
mapToBytes ((sym, bits): cm) = [c2w sym] ++ 
  pairToList (packBits bits) ++ mapToBytes cm    

pairToList :: ([Word8], Int) -> [Word8]
pairToList (byte, pad) = byte ++ [fromIntegral pad]

processEncoding :: Handle -> FilePath -> IO()
processEncoding iHandle output = do
  bString <- BL.hGetContents iHandle
  let contents = BLC8.unpack bString
  let codeMap = buildCodeMap $ contents
  let encoded = encode contents codeMap 
  let (packedBits, pad) = packBits encoded
  let packedMap = mapToBytes codeMap
  bracket
   (openBinaryFile output WriteMode)
   (hClose)
   (\oHandle ->
       do
         BL.hPut oHandle (runPut (putWord8 (fromIntegral (length packedMap))))
         >> BL.hPut oHandle (BL.pack packedMap)
         >> BL.hPut oHandle (runPut (putWord8 (fromIntegral pad))) 
         >> BL.hPut oHandle (BL.pack packedBits) 
         >> putStr "File encoded successfully"
   )

unpackBits :: [Word8] -> Bits
unpackBits bytes = concatMap byteToBits bytes
   
parseFile :: Get (Word8, BL.ByteString, Word8, BL.ByteString)
parseFile = do
  size <- getWord8
  mapString <- getLazyByteString (fromIntegral size)
  pad <- getWord8
  encoded <- getRemainingLazyByteString 
  return (size, mapString, pad, encoded)   
   
processDecoding :: Handle -> FilePath -> IO()
processDecoding iHandle output = do
  bString <- BL.hGetContents iHandle
  let (size, mapString, pad, encoded)  = runGet parseFile bString
  let codeMap = readCodeMap $ BL.unpack mapString
  let unpacked = unpackBits (BL.unpack encoded)
  let bits = take (length unpacked - (fromIntegral pad)) unpacked
  let decoded = decode bits codeMap 
  bracket
   (openBinaryFile output WriteMode)
   (hClose)
   (\oHandle ->
       do
          BL.hPut oHandle (BLC8.pack decoded)
          >> putStr "File decoded successfully"
   )