module Main where

import Lib

main :: IO ()
main = print $ hanoi 4 "a" "b" "c"
