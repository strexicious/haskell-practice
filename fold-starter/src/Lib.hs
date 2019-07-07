module Lib where

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree l = foldr insertTree Leaf l

insertTree :: a -> Tree a -> Tree a
insertTree a Leaf = Node 0 Leaf a Leaf
insertTree a (Node n Leaf x right) = Node (n + 1) (Node 0 Leaf a Leaf) x right
insertTree a (Node n left x right)
  | hl == hr     = Node (height ri + 1) left x ri
  | hr > hl      = Node n li x right
  where hl = height left
        hr = height right
        li = insertTree a left
        ri = insertTree a right

height :: Tree a -> Integer
height Leaf = 0
height (Node _ Leaf _ Leaf) = 0
height (Node _ left _ right) = 1 + max (height left) (height right)

xor :: [Bool] -> Bool
xor l = ((foldr ((+) . boolToInt) 0 l) `mod` 2) == 1

boolToInt :: Bool -> Int
boolToInt n | n         = 1
            | otherwise = 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (applyCons f) []

applyCons :: (a -> b) -> a -> [b] -> [b]
applyCons f a bs = (f a) : bs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\i -> 2*i+1) . filter (flip notElem $ filteredOut) $ [1..n]
    where filteredOut = [i+j+2*i*j | i <- [1..n], j <- [1..n], i <= j, (i+j+2*i*j) <= n]
