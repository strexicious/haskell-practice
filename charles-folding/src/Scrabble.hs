{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Char
import Data.Monoid
    
newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

instance Monoid Score where
    mempty = Score 0
    mappend (Score i1) (Score i2) = Score (i1 + i2)

instance Semigroup Score where
    s1 <> s2 = s1 `mappend` s2

score :: Char -> Score
score c
    | c' `elem` "aeilnorstu" = Score 1
    | c' `elem` "dg"         = Score 2
    | c' `elem` "bcmp"       = Score 3
    | c' `elem` "fhvwy"      = Score 4
    | c' `elem` "k"          = Score 5
    | c' `elem` "jx"         = Score 8
    | c' `elem` "qz"         = Score 10
    | otherwise              = Score 0
    where c' = toLower c

scoreString :: String -> Score
scoreString = foldr1 mappend . map score

getScore :: Score -> Int
getScore (Score x) = x
