module Types where

import Ast.Normal
import Control.Comonad.Cofree
import Data.Fix (Fix(unFix))
import Control.Comonad (Comonad(extract))

newtype TVar = TV String
    deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

data Type
    = TNum
    | TBool
    | TString
    | TVar TVar
    | TScheme Scheme
    | TArr Type Type
    deriving (Show, Eq, Ord)


-- newtype TypedAst = TypedAst (ExprF TypedAst, Type)

type TypedExpr = AnnotatedExpr Type

annotateExpr :: (ExprF (AnnotatedExpr Type) -> AnnotatedExpr Type) -> Expr -> AnnotatedExpr Type
annotateExpr f = annotate f . coerceAnnotation 


annotate ::  (ExprF (Cofree ExprF a) -> Cofree ExprF a) -> Cofree ExprF b -> Cofree ExprF a
annotate func = go
    where
        -- ExprF (Cofree ExprF b)
        -- ExprF (Cofree ExprF a)
        -- Cofree ExprF a

        go = func . fmap go . unwrap

annotate' ::  (ExprF (Cofree ExprF a) -> a) -> Cofree ExprF b -> Cofree ExprF a
annotate' func = annotate (transform func) 
    where transform :: (ExprF (Cofree ExprF a) -> a) -> ExprF (Cofree ExprF a) -> Cofree ExprF a
          transform g e = g e :< e
    -- annotateM :: (Functor f, Monad m) => (f (m (Cofree f a)) -> m )

-- annotateM :: Monad m => (ExprF (Cofree ExprF (m a)) -> m (Cofree ExprF a)) 
--                      -> Cofree ExprF b 
--                      -> m (Cofree ExprF a)
-- annotateM func = go where 
--     -- ExprF (Cofree ExprF b)
--     -- ExprF (m (Cofree ExprF a))
--     fork :: Monad m => ExprF (m (Cofree ExprF a)) -> m (ExprF (Cofree ExprF (m a)))
--     fork m = let label = extract <$> m
--         in undefined
--     go = fmap go . unwrap


-- annotateM :: Monad m => (Cofree ExprF a -> m b) -> Cofree ExprF a -> m (Cofree ExprF b)
-- annotateM func = go where 
--     -- ExprF (Cofree ExprF a)  
--     --   ?
--     -- ExprF (m (Cofree ExprF b))
--     go = fmap go .unwrap