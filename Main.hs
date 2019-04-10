module Main where

import System.Environment
import Expression
import Text.Printf

main :: IO ()
main = do
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = parseExpression input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either concat show a
        putStrLn ""
    )
    fileNames