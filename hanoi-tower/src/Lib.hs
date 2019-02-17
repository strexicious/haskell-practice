module Lib where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ ((a, b) : hanoi (n-1) c b a)

-- a poor attempt at solving four pegs version, got close
-- moves (wrong) recurrence:
-- h(0) = 0
-- h(1) = 1
-- h(n) = 2*h(n-2) + 3
hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 0 _ _ _ _ = []
hanoi' 1 a b _ _ = [(a, b)]
hanoi' n a b c d = hanoi' (n-2) a c b d ++ [(a, d), (a, b), (d, b)] ++ hanoi' (n-2) c b a d
