module Main where

import Party

main :: IO ()
main = readFile "src\\company.txt" >>= putStrLn . glStr . maxFun . read
