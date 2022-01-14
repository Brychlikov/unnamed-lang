{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Ast.Normal where 

import Data.Text(Text)

import Ast.Common
import Data.Fix

data ExprF a 
    = Const Lit 
    | Var Text 
    | Call a a 
    | Let Pattern a a 
    | Lambda Pattern a
    | Cond a a a 
    deriving (Eq, Show, Functor, Foldable, Traversable)

data LetBindingF a
    = Simple Pattern a
    | FunBinding Text Pattern a 
    deriving (Eq, Show, Functor, Foldable, Traversable)

type Expr = Fix ExprF

econst :: Lit -> Expr 
econst = Fix . Const

evar :: Text -> Expr 
evar = Fix . Var 

ecall :: Expr -> Expr -> Expr 
ecall e1 e2 = Fix $ Call e1 e2 

elet :: Pattern ->  Expr -> Expr -> Expr
elet p b e = Fix $ Let p b e

elambda :: Pattern -> Expr -> Expr
elambda pat e = Fix $ Lambda pat e

econd :: Expr -> Expr -> Expr -> Expr
econd e1 e2 e3 = Fix $ Cond e1 e2 e3

