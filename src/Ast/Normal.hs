{-# LANGUAGE DeriveFunctor, DeriveFoldable, TemplateHaskell #-}
module Ast.Normal where 

import Data.Text(Text)

import Ast.Common
import Data.Fix
import Control.Comonad.Cofree
import Data.Functor.Classes (Show1, Eq1)
import Text.Show.Deriving
import Data.Eq.Deriving

data ExprF a 
    = Const Lit 
    | Var Text 
    | Call a a 
    | Let Pattern a a 
    | LFix a
    | Lambda Pattern a
    | Cond a a a 
    | Switch a [(Text, a)]
    deriving (Eq, Show, Functor, Foldable)
$(deriveShow1 ''ExprF)
$(deriveEq1 ''ExprF)

data LetBindingF a
    = Simple Pattern a
    deriving (Eq, Show, Functor, Foldable)
-- | FunBinding Text Pattern a 

type Expr = Fix ExprF
type AnnotatedExpr a = Cofree ExprF a

data Prog = Prog [DataDecl] [LetBindingF Expr]
data AnnotatedProg a = AnnotatedProg [DataDecl] [LetBindingF (AnnotatedExpr a)]

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


elfix :: Expr -> Expr 
elfix e = Fix $ LFix e
-- elfix = ecall (evar "fix")

eswitch :: Expr -> [(Text, Expr)] -> Expr 
eswitch e arms = 
    Fix $ Switch e arms