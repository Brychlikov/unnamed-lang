module Ast.Full where

import Data.Text (Text)

import Ast.Common ( Lit, Op, Pattern )

data Expr
    = Const Lit
    | Var Text
    | Call Expr Expr
    | Binop Op Expr Expr 
    | Neg Expr 
    | Let LetBinding Expr
    | Lambda [Pattern] Expr
    | Cond Expr Expr Expr
    deriving (Eq, Show)


data LetBinding
    = Simple Pattern Expr
    | FunBinding Text [Pattern] Expr
    deriving (Eq, Show)
