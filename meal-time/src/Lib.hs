module Lib where


fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile ((/=) 1) . iterate nextCollatzTerm

nextCollatzTerm :: Integer -> Integer
nextCollatzTerm n
    | even n    = n `div` 2
    | otherwise = 3 * n + 1
