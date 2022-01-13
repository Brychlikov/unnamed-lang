{-# LANGUAGE DeriveFunctor #-}
module Playground where 
import Data.Fix

data ExprF a 
    = One
    | Zero 
    | Val Int 
    | Add a a
    deriving(Show, Functor)

add x y = Expr $ Add x y

newtype Expr = Expr (ExprF Expr) deriving Show

type Expr2 = Fix ExprF

foo :: ExprF Int -> Int
foo (One) = 1
foo (Zero) = 0
foo ((Val x)) = x
foo ((Add x y)) = x + y

coerce :: ExprF a -> ExprF b 
coerce One = One 
coerce Zero = Zero 
coerce (Val x) = Val x
coerce _ = error "cant coerce" 

foldDown :: (ExprF a -> a) -> Expr -> a 
foldDown f (Expr(Add x y)) = f $ Add (foldDown f x) (foldDown f y)
foldDown f (Expr e) = (f . coerce) e