module Main where

import Tokenizer

runTokenizer :: String -> IO ()
runTokenizer input = do
  putStrLn input
  putStrLn $ show $ tokenize input
  putStrLn ""

main :: IO ()
main = do
  runTokenizer " 0b10'10'10 0X2'A 0x2a 052 4'2 asd ewr213423___324 friend char xor "
