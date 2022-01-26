module Ast.Common where 

import Data.Text (Text)

data Op
    = Plus 
    | Minus
    | Mult
    | Div
    | Pair
    | EqEq 
    | Neq
    deriving (Eq, Show)

data Lit
    = Str Text
    | Num Double
    | Boolean Bool
    deriving (Eq, Show)

data Pattern 
    = PVar Text 
    deriving (Eq, Show)

data Type 
    = Con Text 
    | TVar Text
    | App Type Type
    deriving (Eq, Show)

