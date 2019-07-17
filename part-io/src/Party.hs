{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Employee
import Data.List (sort)

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (empFun e + f)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

instance Semigroup GuestList where
    (<>) = mappend

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2)
    | f1 > f2 = g1
    | otherwise = g2

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gll = (gl gll1, glCons e (gl gll2))
    where (gll1, gll2) = unzip gll
          gl l = foldr mappend mempty l

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node r s) = f r (map (treeFold f) s)

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun $ treeFold nextLevel t

glStr :: GuestList -> String
glStr (GL es f) = let sortedNames = unlines . sort . map empName $ es in
    "Total Fun: " ++ show f ++ "\n" ++ sortedNames
