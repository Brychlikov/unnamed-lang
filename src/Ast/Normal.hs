module Ast.Normal where 

import Data.Text(Text)

import Ast.Common

data Expr 
    = Const Lit 
    | Var Text 
    | Call Expr Expr 
    | Let LetBinding Expr 
    | Lambda Pattern Expr
    deriving (Eq, Show)

data LetBinding 
    = Simple Pattern  Expr 
    | FunBinding Text Pattern Expr 
    deriving (Eq, Show)