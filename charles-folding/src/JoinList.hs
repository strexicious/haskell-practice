{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Sized
import Scrabble
import Buffer

data JoinList m a
    = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList
    fromString = foldl (\jl str -> jl +++ scoreLine' str) Empty . lines
        where scoreLine' str = Single (scoreString str, 1) str
    line = indexJ
    replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n+1) jl
    numLines = getSize . snd . tag
    value = getScore . fst . tag

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty j2 = j2
(+++) j1 Empty = j1
(+++) j1 j2    = Append (m1 `mappend` m2) j1 j2
    where m1 = tag j1
          m2 = tag j2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _  ) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ i (Single _ d)
    -- assuming size of Single is always 1
    | i == 0          = Just d
    | otherwise       = Nothing
indexJ i (Append si jl jr)
    | i < 0 || i >= s = Nothing
    | i >= sl         = indexJ (i - sl) jr
    | otherwise       = indexJ i jl
    where sl = getSize . size . tag $ jl
          sr = getSize . size . tag $ jr
          s  = getSize . size $ si

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty   = Empty
dropJ i j@(Single _ _)
    | i >= 1    = Empty
    | otherwise = j
dropJ i j@(Append si jl jr)
    | i <= 0    = j
    | i >= s    = Empty
    | i < sl    = (dropJ i jl) +++ jr
    | otherwise = dropJ (i - sl) jr
    where s  = getSize . size $ si
          sl = getSize . size . tag $ jl

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty   = Empty
takeJ i j@(Single _ _)
    | i >= 1    = j
    | otherwise = Empty
takeJ i j@(Append si jl jr)
    | i >= s    = j
    | i < sl    = takeJ i jl
    | otherwise = jl +++ (takeJ (i - sl) jr)
    where sl = getSize . size . tag $ jl
          s  = getSize . size $ si

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
