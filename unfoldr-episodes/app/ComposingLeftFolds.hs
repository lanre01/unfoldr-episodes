module Main where 

import Data.Char
import Data.Word 

-- -- +RTS -s  

main :: IO ()
main = do
  xs <- readFile "foo.txt"
  let
    ws = words xs

    numWords :: Int
    numWords = length ws

    checksum :: Word8
    checksum = sum $ concatMap (map (fromIntegral . ord)) ws

    wordsTotalLength :: Int
    wordsTotalLength = sum $ map length ws

    averageLength :: Int
    averageLength = wordsTotalLength `div` numWords
  
  putStrLn ("Number of words:     " ++ show numWords)
  putStrLn ("Checksum:            " ++ show checksum)
  putStrLn ("Words total length:  " ++ show wordsTotalLength)
  putStrLn ("Average word length: " ++ show averageLength)