module Types.Infer where

import qualified Data.Map as Map

import Types
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS
import qualified Data.Set as Set
import GHC.RTS.Flags (DoTrace(TraceStderr))
import Ast.Normal
import Data.Fix
import Ast.Common (Lit (Str, Num, Boolean), Pattern (PVar))
import Data.Text (Text)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import Text.Printf (printf)

type Var = Text

newtype TypeEnv = TypeEnv (Map.Map Var Type)

extend :: TypeEnv -> (Var, Type) -> TypeEnv
extend (TypeEnv env) (x, t) = TypeEnv $ Map.insert x t env

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

lookupEnv :: Var -> Infer Type
lookupEnv v = do
    (TypeEnv env) <- ask
    let res = Map.lookup v env
    case res of
        Just (TScheme s) -> instantiate s
        Just t -> return t
        Nothing -> lift $ throwE (UnboundVariable v)

newtype InferState = InferState Int

type Constraint = (Type, Type)

data TypeError
    = TypeMismatch Type Type
    | UnboundVariable Var
    | InfiniteType TVar Type
    | UnificationFail Type Type
    | UnificationMismatch [Type] [Type]
    | MiscError Text 

instance Show TypeError where 
    show (TypeMismatch t1 t2) = printf "TypeError: expected %s, got %s" (show t1) (show t2)
    show (UnboundVariable v)  = printf "UnboundVariable: %s" v
    show (InfiniteType tv t)  = printf "InfiniteType: %s occurs within %s" (show tv) (show t)
    show (UnificationFail t1 t2) = printf "UnificationFail: can't unify %s with %s" (show t1) (show t2)
    show (UnificationMismatch ts1 ts2) = printf "UnificationMismatch: %s doesn't fit %s" (show ts1) (show ts2)
    show (MiscError msg) = printf "MiscError: %s" msg

type Infer  = (RWST
                TypeEnv
                [Constraint]
                InferState
                (Except TypeError)
                )

(~~) :: Type -> Type -> Infer ()
t1 ~~ t2 = tell [(t1, t2)]

fresh :: Infer Type
fresh = do
    (InferState n) <- get
    put $ InferState $ n + 1
    return $ TVar $ TV $ "a" ++ show n

-- we could guarantee values are not schemas by changing Type representation
type Subst = Map.Map TVar Type

emptySubst :: Subst
emptySubst = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.union (Map.map (apply s1) s2) s1

class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set.Set TVar

instance Substitutable Type where
    apply _ TNum = TNum
    apply _ TBool = TBool
    apply _ TString  = TString

    apply s t@(TVar a) = Map.findWithDefault t a s
    apply s (TScheme sc) = TScheme $ apply s sc
    apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)

    ftv TNum  = Set.empty
    ftv TBool   = Set.empty
    ftv TString  = Set.empty

    ftv (TVar a) = Set.singleton a
    ftv (TScheme sc) = ftv sc
    ftv (TArr t1 t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
    apply s (Forall frees t) = Forall frees (apply s' t)
        where s' = foldr Map.delete s frees
    ftv (Forall frees t) = Set.difference (ftv t) (Set.fromList frees)


instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv = foldr (Set.union . ftv ) Set.empty

instance Substitutable TypeEnv where
    apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
    ftv (TypeEnv env) = ftv $ Map.elems env

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where 
    apply = fmap . apply 
    ftv (a, b) = Set.union (ftv a) (ftv b)



instantiate :: Scheme -> Infer Type
instantiate (Forall frees t) = do
    frees' <- mapM (const fresh) frees
    let subst = Map.fromList $ zip frees frees'
    return $ apply subst t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall frees t
    where frees = Set.elems $ ftv t `Set.difference` ftv env

inExtended :: (Var, Type) -> Infer a -> Infer a
inExtended (x, t) m = do
    let modScope env = extend env (x, t)
    local modScope m

litType :: Lit -> Infer Type
litType (Str _    ) = return TString
litType (Num _    ) = return TNum
litType (Boolean _) = return TBool

infer :: Expr -> Infer Type
infer = foldFix collect where
    collect :: ExprF (Infer Type) -> Infer Type

    collect (Const lit) = litType lit
    collect (Var name)  = lookupEnv name
    collect (Call m1 m2) = do
        t1 <- m1
        t2 <- m2
        tv <- fresh
        t1 ~~ TArr t2 tv
        return tv

    collect (Let (PVar x) m1 m2) = do
        env <- ask
        t1 <- m1
        let schema = generalize env t1
        inExtended (x, TScheme schema) m2

    -- collect (Let _ _ _) = lift $ throwE $ MiscError "TODO"

    collect (Lambda (PVar x) m) = do 
        targ <- fresh
        tres <- inExtended (x, targ) m 
        return $ TArr targ tres

    collect (Cond mc mt mf) = do 
        tcond <- mc 
        ttrue <- mt 
        tfalse <- mf 
        tcond ~~ TBool 
        ttrue ~~ tfalse 
        return ttrue

runInfer :: Infer a -> Except TypeError ([Constraint], a)
runInfer i = do 
    (x, y) <- evalRWST i emptyEnv (InferState 0)
    return (y, x)

type Unifier = (Subst, [Constraint])
type Solve = S.StateT Unifier (Except TypeError)

emptyUnifier :: Unifier 
emptyUnifier = (emptySubst, [])

bind :: TVar -> Type -> Solve Unifier 
bind a t | t == TVar a = return emptyUnifier
         | otherwise   = if Set.member a (ftv t) then 
                            lift $ throwE $ InfiniteType a t else 
                            return (Map.singleton a t, [])

unifies :: Type -> Type -> Solve Unifier
unifies t1 t2 | t1 == t2  = return emptyUnifier
unifies (TVar a) t = bind a t
unifies t (TVar a) = bind a t
unifies (TArr t1 t2) (TArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = lift $ throwE $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> Solve Unifier 
unifyMany [] [] = return emptyUnifier
unifyMany (t1: ts1) (t2 : ts2)  = do 
    (uni, constraint) <- unifies t1 t2
    (unis, constraints) <- unifyMany ts1 ts2 
    return (compose uni unis, constraint ++ constraints)
unifyMany t1 t2 = lift $ throwE $ UnificationMismatch t1 t2

solver :: Solve Subst 
solver = do 
    (subst, constraints) <- S.get 
    case constraints of 
        [] -> return subst 
        ((t1, t2) : constraints') -> do 
            (subst2, constraints2) <- unifies t1 t2 
            S.put (compose subst2 subst, constraints2 ++ apply subst2 constraints')
            solver

runSolver :: [Constraint] ->  Except TypeError Subst 
runSolver constraints = fst <$> S.runStateT solver (emptySubst, constraints)


mapTypes :: Expr -> Except TypeError Subst
mapTypes e = do 
    (constraints, t) <- runInfer $ infer e
    runSolver constraints

    
