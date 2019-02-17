module Golf where

import Data.List

-- The output of skips is a list of lists. The first list in the output should be the same as the
-- input list. The second list in the output should contain every second element from the input
-- list... and the nth list in the output should contain every nth element from the input list.
-- 
-- Solution:
-- With every element we include it's index, and then for each element e, we map it to a list
-- which's elements are have the index multiple of e's index
skips :: [a] -> [[a]]
skips l = map (select zl) zl
    where zl = zip l [1..]

stays :: Int -> (a, Int) -> Bool
stays ind (a, pos) = pos `mod` ind == 0

select :: [(a, Int)] -> (a, Int) -> [a]
select l (_, ind) = map fst . filter (stays ind) $ l


-- A local maximum of a list is an element of the list which is strictly greater than both the
-- elements immediately before and after it. For example, in the list[2,3,4,1,5], the only local
-- maximum is 4, since it is greater than the elements immediately before and after it (3 and 1).
-- 5 is not a local maximum since there is no element that comes after it.
-- 
-- Solution:
-- Just build a truth table and you'll figure it out...
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest)
    | x < y && y > z  = [y] ++ localMaxima (z:rest)
    | y < z           = localMaxima (y:z:rest)
    | y > z || y == z = localMaxima (z:rest)
localMaxima _ = []


count :: Eq a => [a] -> a -> Int
count l a = length . filter (a ==) $ l

toStars :: Show a => Int -> (Int, a) -> String
toStars m (n, i) = replicate (m-n) ' ' ++ replicate n '*' ++ "=" ++ (show i)

histogram :: [Integer] -> String
histogram h = unlines (transpose (map (toStars . maximum $ map fst c) c))
    where c = map (\x -> (count h x, x)) [0..9]
