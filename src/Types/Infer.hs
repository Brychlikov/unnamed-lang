{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Types.Infer where

import qualified Data.Map as Map

import Types
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.CPS
import qualified Data.Set as Set
import GHC.RTS.Flags (DoTrace(TraceStderr))
import Ast.Normal
import Data.Fix
import Ast.Common (Lit (Str, Num, Boolean), Pattern (PVar), )
import qualified Ast.Common as C
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import Text.Printf (printf)
import Control.Comonad.Cofree (ComonadCofree(unwrap), Cofree ((:<)), section)
import Control.Comonad (extract)
import Debug.Trace

import Text.Pretty.Simple
import Data.Text.Lazy (toStrict)
import Control.Applicative (Const)
import Data.Foldable (foldrM)
import Data.Maybe (fromMaybe, fromJust)
import Data.Bifunctor (second)
import GHC.Base (thenIO)
import Data.List (find)

type Var = Text

newtype TypeEnv = TypeEnv {unTypeEnv :: (Map.Map Var Type, [Constructor])} deriving Show

traceMsg :: Show a => String -> a -> a
-- traceMsg s a = trace (s ++ show a) a
traceMsg s a = a
traceMsgWith :: (a -> String) -> String -> a -> a
traceMsgWith f msg a = trace (msg ++ ": " ++ f a) a
traceMsgWith' :: (a -> Text) -> Text -> a -> a
traceMsgWith' f msg a = trace (unpack (msg `T.append` ": " `T.append` f a)) a


extend :: TypeEnv -> (Var, Type) -> TypeEnv
extend (TypeEnv (env, cs)) (x, t) = TypeEnv (Map.insert x t env, cs)

emptyEnv :: TypeEnv
emptyEnv = TypeEnv (Map.empty, [])

tupleApplication :: Type -> Type -> Type
tupleApplication t1 = TApp (TApp (TCon tup) t1)

applyConstructor  :: Constructor -> [Type] -> Type
applyConstructor = foldl TApp . TCon

evalConDeclaration :: [Constructor] -> C.ConDecl -> Either TypeError ConDecl
evalConDeclaration cs (C.ConDecl cname ctypes) = do
    types <- mapM (evalType cs) ctypes
    return $ ConDecl cname types


evalDataDeclaration :: [Constructor] -> C.DataDecl -> Either TypeError DataDecl
evalDataDeclaration cs (C.DataDecl name varnames consts) = do
    let dKind   = foldr (\el acc -> KType `KArr` acc) KType varnames
        newCon  = Constructor {name = name, kind = dKind}
        newType = foldl (\acc el -> acc `TApp` TVar (TV el)) (TCon newCon) varnames

    condecs <- mapM (evalConDeclaration (newCon : cs)) consts
    return $ DataDecl newCon newType condecs

makeFunc :: [Type] -> Type -> Type
makeFunc ts restype = generalize' emptyEnv $ foldr tArr restype ts

conType :: DataDecl -> ConDecl -> Type
conType (DataDecl con tp _ ) (ConDecl cname ts) =
    traceMsgWith show ("making constructor for " ++ unpack cname) $ makeFunc ts tp
    -- makeFunc ts tp

conPred :: DataDecl -> ConDecl -> Type
conPred (DataDecl con tp _) (ConDecl cname ts) =
    makeFunc [tp] tBoolean

conGetters :: DataDecl -> ConDecl -> [(Text, Type)]
conGetters (DataDecl con tp constrs) (ConDecl cname ts) =
    [
        ("get" `T.append` cname `T.append` T.pack (show n), makeFunc [tp] t)
    |   (n, t) <- zip [0..] ts
    ]



addDataDeclaration' :: DataDecl -> TypeEnv -> TypeEnv
addDataDeclaration' dd@(DataDecl con tp constrs) (TypeEnv (env, cs)) = TypeEnv (env''', cs') where
    cs' = con : cs
    env' = foldl (\e cd@(ConDecl name ts) -> Map.insert name (conType dd cd) e) env constrs
    env'' = foldl (\e cd@(ConDecl name ts) -> Map.insert ("is" `T.append` name) (conPred dd cd) e) env' constrs
    env''' = foldl (\e cd@(ConDecl name ts) -> Map.fromList (conGetters dd cd) `Map.union` e) env'' constrs

-- addDataDeclaration :: C.DataDecl -> TypeEnv -> Either TypeError TypeEnv
-- addDataDeclaration (C.DataDecl name varnames consts) (TypeEnv (tenv, kinds)) =  res where
--     dKind = foldr (\el acc -> KType `KArr` acc) KType varnames
--     newCon = Constructor {name = name, kind = dKind}

--     newType = foldl (\acc el -> acc `TApp` TVar (TV el)) (TCon newCon) varnames


--     foldInner :: Type -> C.ConDecl -> Map.Map Text Type -> Either TypeError (Map.Map Text Type)
--     foldInner restype (C.ConDecl cname ctypes) prevMap = do
--         types <- mapM (evalType (newCon : kinds)) ctypes
--         let fType = makeFunc types restype
--         return $ Map.insert cname fType prevMap

--     tenvM = foldrM
--         (foldInner newType)
--         tenv
--         consts


--     res = tenvM >>= \tenv' -> Right $ TypeEnv (tenv', newCon : kinds)




startingEnv :: TypeEnv
startingEnv = TypeEnv (map, [])
    where
    map = Map.fromList
        [ ("(+)", tArr tNum (tArr tNum tNum))
        , ("(-)", tArr tNum (tArr tNum tNum))
        , ("(*)", tArr tNum (tArr tNum tNum))
        , ("(/)", tArr tNum (tArr tNum tNum))
        , ("(==)", TScheme (Forall [TV "a"]
                                (tArr (TVar $ TV "a") (tArr (TVar $ TV "a") tBoolean))))
        , ("(!=)", TScheme (Forall [TV "a"]
                                (tArr (TVar $ TV "a") (tArr (TVar $ TV "a") tBoolean))))
        , ("(,)", TScheme (Forall [TV "a", TV "b"]
                                (tArr (TVar $ TV "a")
                                        (tArr (TVar $ TV "b")
                                            (tupleApplication (TVar $ TV "a") (TVar $ TV "b") )))))
        , ("magic", TScheme (Forall [TV "a", TV "b"]
                                (tArr (TVar $ TV "a")
                                        (TVar $ TV "b") )))
        , ("print", TScheme (Forall [TV "a"] (tArr (TVar $ TV "a") tUnit)))
        , ("fst", TScheme (Forall [TV "a", TV "b"]
                                (tArr (tupleApplication (TVar $ TV "a") (TVar $ TV "b") )
                                        (TVar $ TV "a"))))
        , ("snd", TScheme (Forall [TV "a", TV "b"]
                                (tArr (tupleApplication (TVar $ TV "a") (TVar $ TV "b") )
                                        (TVar $ TV "b"))))
        ]

lookupEnv :: Var -> Infer Type
lookupEnv v = do
    (TypeEnv (env, cs)) <- ask
    let res = Map.lookup v env
    case res of
        Just (TScheme s) -> instantiate s
        Just t -> return t
        Nothing -> lift $ throwE (UnboundVariable v)

newtype InferState = InferState Int

displayKind :: Kind -> String
displayKind KType = "*"
displayKind (KArr k1 k2) = "(" ++ displayKind k1 ++ " => " ++ displayKind k2 ++ ")"

displayType :: Type -> String
displayType (TVar (TV x)) =  unpack x
displayType (TScheme s) = show s
-- displayType (TArr t1 t2) = "(" ++ displayType t1 ++ "->" ++ displayType t2 ++ ")"
displayType (TCon constr) = (unpack . name) constr
displayType (TApp t1 t2) = "(" ++ displayType t1 ++  " " ++ displayType t2 ++ ")"

type Constraint = (Type, Type)

displayConstraint :: Constraint -> String
displayConstraint (t1, t2) = displayType t1  ++ "~~" ++ displayType t2

displayConstraints :: [Constraint] -> String
displayConstraints = (++"]") . foldl (\s c -> s ++ displayConstraint c ++ ",") "["


type Infer  = (RWST
                TypeEnv
                [Constraint]
                InferState
                (Except TypeError)
                )

(~~) :: Type -> Type -> Infer ()
t1 ~~ t2 = tell   [(t1, t2)]

fresh :: Infer Type
fresh = do
    (InferState n) <- get
    put $ InferState $ n + 1
    return $ TVar $ TV $ "a" `T.append` (T.pack . show) n

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
    apply s t@(TVar a) = Map.findWithDefault t a s
    apply s (TScheme sc) = TScheme $ apply s sc
    apply s (TCon c) = TCon c
    apply s (TApp t1 t2) = TApp (apply s t1) (apply s t2)

    ftv (TVar a) = Set.singleton a
    ftv (TScheme sc) = ftv sc
    ftv (TCon _) = Set.empty
    -- TODO: Something fishy here, don't know how type applications should work yet
    ftv (TApp t1 t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
    apply s (Forall frees t) = Forall frees (apply s' t)
        where s' = foldr Map.delete s frees
    ftv (Forall frees t) = Set.difference (ftv t) (Set.fromList frees)


instance Substitutable a => Substitutable [a] where
    apply = fmap . apply
    ftv = foldr (Set.union . ftv ) Set.empty
-- instance (Functor f, Foldable f, Substitutable a) => Substitutable (f a) where 
--     apply = fmap . apply
--     ftv = foldr (Set.union . ftv ) Set.empty

instance Substitutable a => Substitutable  (LetBindingF a) where
    apply s (Simple pat e) = Simple pat (apply s e)
    ftv (Simple pat e ) = ftv e


instance Substitutable (AnnotatedProg Type) where
    apply s (AnnotatedProg datas lets) = AnnotatedProg datas (map (apply s) lets)
    ftv (AnnotatedProg datas lets) = foldr (Set.union . ftv) Set.empty lets



instance Substitutable (Cofree ExprF Type) where
    apply = fmap . apply
    -- Cofree ExprF SV
    ftv = foldMap ftv


instance Substitutable TypeEnv where
    apply s (TypeEnv (env, cs)) = TypeEnv  (Map.map (apply s) env, cs)
    ftv (TypeEnv (env, cs)) = ftv $ Map.elems env

-- apply :: Subst -> a -> a
instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
    -- źle
    -- apply = fmap . apply 

    -- dobrze 
    apply s (a, b) = (apply s a, apply s b)

    ftv (a, b) = Set.union (ftv a) (ftv b)



instantiate :: Scheme -> Infer Type
instantiate (Forall frees t) = do
    frees' <- mapM (const fresh) frees
    let subst = Map.fromList $ zip frees frees'
    return $ apply subst t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall frees t
    where frees = Set.elems $ ftv t `Set.difference` ftv env

generalize' :: TypeEnv -> Type -> Type
generalize' env t =
    if null frees then
        t
    else
        TScheme $ Forall frees t
    where
        frees = Set.elems $ ftv t `Set.difference` ftv env


inExtended :: (Var, Type) -> Infer a -> Infer a
inExtended (x, t) m = do
    let modScope env = extend env (x, t)
    local modScope m

litType :: Lit -> Type
litType (Str _    ) = tString
litType (Num _    ) = tNum
litType (Boolean _) = tBoolean

-- infer, but sad and basic :(
inferSB :: Expr -> Infer (AnnotatedExpr Type)
inferSB = inner . unwrap . coerceAnnotation where

    inner :: ExprF (Cofree ExprF ()) -> Infer (Cofree ExprF Type)
    inner (Const lit) = return $ litType lit :< Const lit
    inner (Var name) = do
        t <- lookupEnv name
        return $ t :< Var name
    inner (Call e1 e2) = do
        sub1@(ty1 :< tr1) <- inner (unwrap e1)
        sub2@(ty2 :< tr2) <- inner (unwrap e2)
        tv <- fresh
        ty1 ~~ tArr ty2 tv
        let tree = Call sub1 sub2
        return $ tv :< tree

    inner (Let (PVar x) e1 e2) = do
        env <- ask
        binder@(t1 :< _) <- inner (unwrap e1)
        let schema = generalize env t1
        res@(rt :< _) <- inExtended (x, TScheme schema) (inner $ unwrap e2)
        return $ rt :< Let (PVar x) binder res

    inner (Lambda (PVar x) e) = do
        tp <- fresh
        res@(rt :< _) <- inExtended (x, tp) (inner $ unwrap e)
        return $ tArr tp rt :< Lambda (PVar x) res

    inner (Cond ec et ef) = do
        trcond@(tc :< _) <- inner $ unwrap ec
        trtrue@(tt :< _) <- inner $ unwrap et
        trfalse@(tf :< _) <- inner $ unwrap ef
        tc ~~ tBoolean
        tt ~~ tf
        return $ tt :< Cond trcond trtrue trfalse

    inner (LFix name e) = do
        tres <- fresh
        trlam@(ltype :< _) <- inExtended (name, tres) (inner $ unwrap e)
        -- ltype ~~ TArr (TArr tres tres) tres
        ltype ~~ tres
        return $ tres :< LFix name trlam

    inner (Switch e arms) = do
        matchee@(subtype :< _) <- inner $ unwrap e
        pattype <- getPatType (map fst arms)
        subtype ~~ pattype
        tres <- fresh
        let armLabels = map fst arms
        typedArms <-  mapM (inner . unwrap . snd) arms
        let armTypes = map getType typedArms
        mapM_ (tres~~) armTypes

        return $ tres :< Switch matchee (zip armLabels typedArms)
        where
            getPatType :: [Text] -> Infer Type
            getPatType labels = do
                ts <- mapM getConType labels
                case ts of
                    []  -> lift $ throwE $ MiscError "Empty matches not allowed, this should be unreachable"
                    h:t -> if all (typeEquality h) ts then return h
                        else let offender = find (not . typeEquality h) ts in
                            lift $ throwE $ MiscError $
                                "Con mismatch on " `T.append` (T.pack $ show labels) `T.append` ": " `T.append` T.pack (show h) `T.append` " vs " `T.append` (T.pack $ show $ fromJust offender)


            getConType :: Text -> Infer Type
            getConType cname = do
                res <- asks (Map.lookup cname . fst . unTypeEnv)
                case res of
                    Just t -> return $ (traceShowId $ getRetType t)
                    Nothing -> lift $ throwE $ MiscError "aaaaaaaaaa"

            getRetType (TApp (TApp (TCon c) t2) t) | c == cArrow = getRetType t
            -- getRetType (TScheme (Forall vars t)) = TScheme (Forall vars (getRetType t))
            getRetType (TScheme (Forall vars t)) = getRetType t
            getRetType t = t

            typeEquality :: Type -> Type -> Bool
            typeEquality t1 t2 = case runExcept $ runSolver [(t1, t2)] of 
                Left _ -> False
                Right _ -> True

            getType (t :< _) = t
    inner _ = undefined




-- infer :: Expr -> Infer (AnnotatedExpr Type)
-- infer = undefined . annotate' func . coerceAnnotation where
--     func :: ExprF (AnnotatedExpr (Infer Type)) -> Infer Type
--     func = collect . fmap extract
--     collect :: ExprF (Infer Type) -> Infer Type

--     collect (Const lit) = litType lit
--     collect (Var name)  = lookupEnv name
--     collect (Call m1 m2) = do
--         t1 <- m1
--         t2 <- m2
--         tv <- fresh
--         t1 ~~ TArr t2 tv
--         return tv

--     collect (Let (PVar x) m1 m2) = do
--         env <- ask
--         t1 <- m1
--         let schema = generalize env t1
--         inExtended (x, TScheme schema) m2

--     -- collect (Let _ _ _) = lift $ throwE $ MiscError "TODO"

--     collect (Lambda (PVar x) m) = do 
--         targ <- fresh
--         tres <- inExtended (x, targ) m 
--         return $ TArr targ tres

--     collect (Cond mc mt mf) = do 
--         tcond <- mc 
--         ttrue <- mt 
--         tfalse <- mf 
--         tcond ~~ TBool 
--         ttrue ~~ tfalse 
--         return ttrue

tryTE :: Either TypeError a -> Infer a
tryTE (Right a) = return a
tryTE (Left err) = lift $ throwE err

inferProg :: Prog -> Infer ([DataDecl], [LetBindingF TypedExpr])
inferProg (Prog datas lets) = do

    (constrs, datadecls) <- tryTE $ foldrM (\d (cs, res) -> do
            dd@(DataDecl c tp cds) <- evalDataDeclaration cs d
            return (c:cs, dd:res) ) ([], []) datas

    let TypeEnv (beginEnv, cons) = foldr addDataDeclaration' startingEnv datadecls

    beginEnv' <- foldrM (\(Simple pat _ ) acc -> do
        tv <- fresh
        case pat of
            C.PNull -> return acc
            C.PVar name -> return $ Map.insert name tv acc
            C.PCon _ _ -> error "TODO"
        )
        beginEnv
        lets

    typedLets <-mapM (\(Simple pat te) -> do
             tv <- case pat of
                 C.PVar name -> local (const $ TypeEnv (beginEnv', cons)) $ asks (fromJust . Map.lookup name . fst . unTypeEnv)
                 C.PNull -> fresh
                 C.PCon _ _ -> error "TODO"
             tres :< resTree <- local (const $ TypeEnv (beginEnv', cons)) $ inferSB te
             tv ~~ tres
             return $ Simple pat (tres :< resTree)
             )
           lets
    return (datadecls, typedLets)

runInfer :: Infer a -> Except TypeError ([Constraint], a)
runInfer i = do
    (x, y) <- evalRWST i startingEnv (InferState 0)
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
unifies (TApp t1 t2) (TApp t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = lift $ throwE $ UnificationFail t1 t2

unifyMany :: [Type] -> [Type] -> Solve Unifier
unifyMany [] [] = return emptyUnifier
unifyMany (t1: ts1) (t2 : ts2)  = do
    (uni, constraint) <- unifies t1 t2
    (unis, constraints) <- unifyMany ts1 ts2
    let newSubst = compose uni unis
        newConstraint = constraint ++ constraints
    let tup = (newSubst, newConstraint)
    return tup
unifyMany t1 t2 = lift $ throwE $ UnificationMismatch t1 t2


spShow :: Show a => a -> String
spShow = unpack . toStrict . pShow

solver :: Solve Subst
solver = do
    (subst, constraints) <- S.get
    case constraints of
        [] -> return subst
        ((t1, t2) : constraints') -> do
            (subst2, constraints2) <- unifies t1 t2
            let newSubstitution = compose subst2 subst
            let oldConstraints = apply newSubstitution constraints'
            let newConstraints = constraints2 ++ oldConstraints
            S.put (newSubstitution, newConstraints)
            solver

runSolver :: [Constraint] ->  Except TypeError Subst
runSolver constraints = fst <$> S.runStateT solver (emptySubst, constraints)


-- mapTypes :: Expr -> Except TypeError Subst
-- mapTypes e = do 
--     (constraints, t) <- runInfer $ infer e
--     runSolver constraints


typeExpr :: Expr -> Except TypeError TypedExpr
typeExpr e = do
    (constraints, tree) <- runInfer $ inferSB e
    subst <- runSolver constraints
    return $ apply subst tree

typeProg :: Prog -> Except TypeError ([DataDecl], [LetBindingF TypedExpr])
typeProg p@(Prog datas lets) = do
    (constraints, (evaledDatas, typedLets)) <- runInfer $ inferProg p
    subst <- runSolver constraints
    let solvedLets = apply subst typedLets
    return (evaledDatas, solvedLets)

