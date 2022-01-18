{-# LANGUAGE DeriveFunctor, DeriveFoldable, TemplateHaskell #-}
module Ast.Normal where 

import Data.Text(Text)

import Ast.Common
import Data.Fix
import Control.Comonad.Cofree
import Data.Functor.Classes (Show1)
import Text.Show.Deriving

data ExprF a 
    = Const Lit 
    | Var Text 
    | Call a a 
    | Let Pattern a a 
    | Lambda Pattern a
    | Cond a a a 
    deriving (Eq, Show, Functor, Foldable)
$(deriveShow1 ''ExprF)

data LetBindingF a
    = Simple Pattern a
    | FunBinding Text Pattern a 
    deriving (Eq, Show, Functor, Foldable)

type Expr = Fix ExprF
type AnnotatedExpr a = Cofree ExprF a

coerceAnnotation :: Expr -> AnnotatedExpr () 
coerceAnnotation = foldFix ann where 
    ann :: ExprF (AnnotatedExpr () ) -> AnnotatedExpr () 
    ann e = () :< e

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

