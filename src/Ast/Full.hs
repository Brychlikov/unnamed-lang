module Ast.Full where

import Data.Text (Text)

import Ast.Common 

data Expr
    = Const Lit
    | Var Text
    | Call Expr Expr
    | Binop Op Expr Expr 
    | Neg Expr 
    | Let LetBinding Expr
    | Lambda [Pattern] Expr
    | Cond Expr Expr Expr
    | Match Expr [(Pattern, Expr)]
    deriving (Eq, Show)


data LetBinding
    = Simple Pattern Expr
    | FunBinding Text [Pattern] Expr
    deriving (Eq, Show)


data Decl
    = LDecl LetBinding 
    | DDecl  DataDecl
    deriving (Eq, Show)

newtype Prog = Prog [Decl] 
    deriving (Eq, Show)