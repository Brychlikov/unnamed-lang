module Types where

import Ast.Normal
import qualified Ast.Common as C
import Control.Comonad.Cofree
import Data.Fix (Fix(unFix))
import Control.Comonad (Comonad(extract))
import Data.Text (Text, unpack)
import Data.List (find)
import Text.Printf (printf)

newtype TVar = TV Text
    deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
    deriving (Show, Eq, Ord)

data Kind
    = KType
    | KArr Kind Kind
    deriving (Show, Eq, Ord)


data Constructor = Constructor
    { name :: Text
    , kind :: Kind
    } deriving (Show, Eq, Ord)

tup :: Constructor
tup = Constructor
    { name = "(,)"
    , kind = KType `KArr` (KType `KArr` KType)
}

cNum :: Constructor
cNum = Constructor
    { name = "Number"
    , kind = KType
    }

cBoolean :: Constructor
cBoolean = Constructor
    { name = "Boolean"
    , kind = KType
    }

cString :: Constructor
cString = Constructor
    { name = "String"
    , kind = KType
    }

cUnit :: Constructor
cUnit = Constructor
    { name = "Unit"
    , kind = KType
    }

tNum :: Type
tNum = TCon cNum

tBoolean :: Type
tBoolean = TCon cBoolean

tString :: Type
tString = TCon cString

tUnit :: Type
tUnit = TCon cUnit

cArrow :: Constructor
cArrow = Constructor
    { name = "(->)"
    , kind = KType `KArr` (KType `KArr` KType)
    }

cList :: Constructor 
cList = Constructor "List" (KType `KArr` KType)

builtinConstrs :: [Constructor]
builtinConstrs = [cBoolean, cString, cNum, cUnit, cArrow, cList]

tArr :: Type -> Type -> Type
tArr t1 t2 = TApp (TApp (TCon cArrow) t1) t2

checkKind :: Type -> Kind
checkKind (TCon con) = kind con
checkKind (TApp t1 t2) = case k1 of
    KType      -> error "kind error"
    KArr k3 k4 -> if k3 == k2 then k4 else error "kind error"
    where
        k1 = checkKind t1
        k2 = checkKind t2
checkKind _ = KType


data Type
    = TVar TVar
    | TScheme Scheme
    | TCon Constructor
    | TApp Type Type
    deriving (Show, Eq, Ord)


data DataDecl = DataDecl Constructor Type [ConDecl]
data ConDecl = ConDecl Text [Type]

data TypeError
    = TypeMismatch Type Type
    | UnboundVariable Text
    | InfiniteType TVar Type
    | UnificationFail Type Type
    | UnificationMismatch [Type] [Type]
    | KindError C.Type Kind Kind
    | UnknownType Text
    | MiscError Text

instance Show TypeError where
    show (TypeMismatch t1 t2) = printf "TypeError: expected %s, got %s" (displayType t1) (displayType t2)
    show (UnboundVariable v)  = printf "UnboundVariable: %s" v
    show (InfiniteType tv t)  = printf "InfiniteType: %s occurs within %s" (show tv) (displayType t)
    show (UnificationFail t1 t2)       = printf "UnificationFail: can't unify %s with %s" (displayType t1) (displayType t2)
    show (UnificationMismatch ts1 ts2) = printf "UnificationMismatch: %s doesn't fit %s" (show ts1) (show ts2)
    show (MiscError msg)      = printf "MiscError: %s" msg
    show (KindError ct k1 k2)     = printf "%s was expected to be of kind %s, but has kind %s" (show ct) (show k1) (show k2)
    show (UnknownType t)      = printf "UnboundType: %s" (show t)
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

evalType :: [Constructor] -> C.Type -> Either TypeError Type
evalType = inner KType where 
    inner ex constrs (C.Con cName) = do 
        con <- maybe (Left $ UnknownType cName) Right (find (\c -> name c == cName) constrs)
        if kind con == ex then 
            Right $ TCon con
        else 
            Left $ KindError (C.Con cName) (kind con) ex

    inner ex _ (C.TVar name) = Right $ TVar $ TV name

    inner ex constrs ct@(C.App ct1 ct2) = do
        t1 <- inner (KType `KArr` ex) constrs ct1 
        t2 <- inner KType constrs ct2 
        Right $ TApp t1 t2


displayKind :: Kind -> String
displayKind KType = "*"
displayKind (KArr k1 k2) = "(" ++ displayKind k1 ++ " => " ++ displayKind k2 ++ ")"

displayType :: Type -> String
displayType (TVar (TV x)) =  unpack x
displayType (TScheme (Forall vars t)) = "∀" ++ concatMap (\(TV x) -> unpack x) vars ++ displayType t
-- displayType (TArr t1 t2) = "(" ++ displayType t1 ++ "->" ++ displayType t2 ++ ")"
displayType (TCon constr) = (unpack . name) constr
displayType (TApp t1 t2) = "(" ++ displayType t1 ++  " " ++ displayType t2 ++ ")"

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