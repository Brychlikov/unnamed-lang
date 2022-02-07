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
    | Seq
    | Concat
    deriving (Eq, Show)

data Lit
    = Str Text
    | Num Double
    | Boolean Bool
    | Unit
    deriving (Eq, Show)

data Pattern 
    = PVar Text 
    | PNull 
    | PCon Text [Text]
    deriving (Eq, Show)

data Type 
    = Con Text 
    | TVar Text
    | App Type Type
    deriving (Eq, Show)

data DataDecl = DataDecl Text [Text] [ConDecl]
    deriving(Eq, Show)

data ConDecl = ConDecl Text [Type]
    deriving(Eq, Show)