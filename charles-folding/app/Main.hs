module Main where

import Editor
import Buffer
import JoinList
import Sized
import Scrabble

main :: IO ()
main = runEditor editor (fromString "test" :: (JoinList (Score, Size) String))
