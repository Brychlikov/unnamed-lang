module Ast where

import Data.Text (Text)

data Expr
    = Const Lit
    | Var Text
    | Call Expr Expr
    | Binop Op Expr Expr 
    | Neg Expr 
    | Let LetBinding Expr
    | Lambda [Pattern] Expr
    deriving (Eq, Show)

data Op
    = Plus 
    | Minus
    | Mult
    | Div
    deriving (Eq, Show)

data Lit
    = Str Text
    | Num Float
    deriving (Eq, Show)

data Pattern 
    = PVar Text 
    deriving (Eq, Show)

data LetBinding
    = Simple Pattern Expr
    | FunBinding Text [Pattern] Expr
    deriving (Eq, Show)