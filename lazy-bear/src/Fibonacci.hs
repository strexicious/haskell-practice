{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib n | n <- [0..]]

fibs2 :: [Integer]
fibs2 = [0, 1] ++ zipWith (+) fibs2 (tail fibs2)

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
    show s = show . take 20 $ (streamToList s)

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a s) = Stream (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a (streamFromSeed f (f a))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a1 s1) s2 = Stream a1 (interleaveStreams s2 s1)

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = foldr1 interleaveStreams $ map streamRepeat [0..]

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Stream n (streamRepeat 0)
    negate = streamMap negate
    (Stream a s1) + (Stream b s2) = Stream (a + b) (s1 + s2)
    (Stream a s1) * bs@(Stream b s2) = Stream (a * b) ((streamMap (* a) s2) + (s1 * bs))

instance Fractional (Stream Integer) where
    (/) (Stream a s1) (Stream b s2) = q
        where q = Stream (a `div` b) (streamMap (`div` b) (s1 - q * s2))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    (Matrix a11 b11 a21 b21) * (Matrix a12 b12 a22 b22) =
        Matrix (a11 * a12 + b11 * a22) (a11 * b12 + b11 * b22)
               (a21 * a12 + b21 * a22) (a21 * b12 + b21 * b22)

fib1m :: Matrix
fib1m = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 0
fib4 n = a
    where (Matrix a _ _ _) = fib1m ^ (n-1)
