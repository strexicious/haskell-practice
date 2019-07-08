{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}

module Calc where

import qualified ExprT as ExprT
import Parser
import StackVM (Program)
import qualified StackVM as VM

import qualified Data.Map as M

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
    deriving (Show, Eq)

class Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e

instance Expr ExprT.ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit n = if n <= 0 then False else True
    add   = (||)
    mul   = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit                       = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)


instance Expr Mod7 where
    lit n                 = Mod7 (n `mod` 7)
    add (Mod7 a) (Mod7 b) = lit (a + b)
    mul (Mod7 a) (Mod7 b) = lit (a * b)

instance Expr Program where
    lit n = [VM.PushI n]
    add l r = l ++ r ++ [VM.Add]
    mul l r = l ++ r ++ [VM.Mul]

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = \_ -> Just n
    add f g = \m -> (f m) >>= (\x -> (g m) >>= \y -> Just (x + y))
    mul f g = \m -> (f m) >>= (\x -> (g m) >>= \y -> Just (x * y))

class HasVars a where
    var :: String -> a

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

compile :: String -> Maybe Program
compile = parseExp lit add mul

eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit n)   = n
eval (ExprT.Add a b) = (eval a) + (eval b)
eval (ExprT.Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

reify :: ExprT.ExprT -> ExprT.ExprT
reify = id

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
