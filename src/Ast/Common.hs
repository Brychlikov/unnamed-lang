module Ast.Common where 

import Data.Text (Text)

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
