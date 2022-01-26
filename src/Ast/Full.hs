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
    deriving (Eq, Show)


data LetBinding
    = Simple Pattern Expr
    | FunBinding Text [Pattern] Expr
    deriving (Eq, Show)

data DataDecl = DataDecl Text [Text] [ConDecl]
    deriving(Eq, Show)

data ConDecl = ConDecl Text [Type]
    deriving(Eq, Show)

data Decl
    = LDecl LetBinding 
    | DDecl  DataDecl
    deriving (Eq, Show)

data Prog = Prog [Decl] Expr