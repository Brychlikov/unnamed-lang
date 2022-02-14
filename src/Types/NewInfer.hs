{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Types.NewInfer where

import qualified Data.Map as Map
import Types
import Control.Monad.Trans.Except
import qualified Data.Set as Set

import Ast.Normal
import Data.Fix
import Ast.Common (Lit (Str, Num, Boolean, Unit), Pattern (PVar, PNull, PCon), )
import qualified Ast.Common as C
import Data.Text (Text, unpack)
import qualified Data.Text as T

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S
import Text.Printf (printf)
import Control.Comonad.Cofree (ComonadCofree(unwrap), Cofree ((:<)), section)
import Control.Comonad (extract)
import Debug.Trace
import Control.Monad.State (StateT (runStateT), evalStateT)
import Data.Bifunctor
import Data.Foldable (foldrM, foldlM)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Map.Merge.Strict


traceMsgWith :: (a -> String) -> String -> a -> a
traceMsgWith f msg a = trace (msg ++ ": " ++ f a) a


data TypeEnv = TypeEnv
    { tenv :: Map.Map Text Scheme
    , ctrs :: [Constructor]
    }



extend :: TypeEnv -> (Text, Scheme) -> TypeEnv
extend TypeEnv{tenv, ctrs} (n, s) = TypeEnv (Map.insert n s tenv) ctrs

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty builtinConstrs

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


makeFuncEmpty :: [Type] -> Type -> Scheme
makeFuncEmpty ts restype = generalize emptyEnv $ foldr tArr restype ts

makeFunc :: TypeEnv -> [Type] -> Type -> Scheme
makeFunc env ts restype = generalize env $ foldr tArr restype ts


conType :: DataDecl -> ConDecl -> Scheme
conType (DataDecl con tp _ ) (ConDecl cname ts) =
    -- traceMsgWith displayScheme ("making constructor for " ++ unpack cname) $ makeFunc ts tp
    makeFuncEmpty ts tp

conPred :: DataDecl -> ConDecl -> Scheme
conPred (DataDecl con tp _) (ConDecl cname ts) =
    makeFuncEmpty [tp] tBoolean

conGetters :: DataDecl -> ConDecl -> [(Text, Scheme)]
conGetters (DataDecl con tp constrs) (ConDecl cname ts) =
    [
        ("get" `T.append` cname `T.append` T.pack (show n), makeFuncEmpty [tp] t)
    |   (n, t) <- zip [0..] ts
    ]

startingEnv :: TypeEnv
startingEnv = TypeEnv map builtinConstrs
    where
    map = Map.fromList
        [ ("(+)", Forall [] $ tArr tNum (tArr tNum tNum))
        , ("(-)", Forall [] $ tArr tNum (tArr tNum tNum))
        , ("(*)", Forall [] $ tArr tNum (tArr tNum tNum))
        , ("(/)", Forall [] $ tArr tNum (tArr tNum tNum))
        , ("(++)", Forall [] $ tArr tString (tArr tString tString))
        , ("(==)", Forall [TV "a"]
                                (tArr (TVar $ TV "a") (tArr (TVar $ TV "a") tBoolean)))
        , ("(!=)", Forall [TV "a"]
                                (tArr (TVar $ TV "a") (tArr (TVar $ TV "a") tBoolean)))
        , ("(,)", Forall [TV "a", TV "b"]
                                (tArr (TVar $ TV "a")
                                        (tArr (TVar $ TV "b")
                                            (tupleApplication (TVar $ TV "a") (TVar $ TV "b") ))))
        , ("magic", Forall [TV "a", TV "b"]
                                (tArr (TVar $ TV "a")
                                        (TVar $ TV "b") ))
        , ("print", Forall [TV "a"] (tArr (TVar $ TV "a") tUnit))
        , ("println", Forall [TV "a"] (tArr (TVar $ TV "a") tUnit))
        , ("fst", Forall [TV "a", TV "b"]
                                (tArr (tupleApplication (TVar $ TV "a") (TVar $ TV "b") )
                                        (TVar $ TV "a")))
        , ("snd", Forall [TV "a", TV "b"]
                                (tArr (tupleApplication (TVar $ TV "a") (TVar $ TV "b") )
                                        (TVar $ TV "b")))
        , ("(;)", Forall [TV "a"]
                                (tArr tUnit ((TVar $ TV "a") `tArr` (TVar $ TV "a"))))
        , ("error", Forall [TV "a"] $ tString `tArr` TVar (TV "a"))
        , ("numToChar", Forall [] (tNum`tArr` tString))
        , ("charList", Forall [] (tString`tArr` TApp (TCon cList) tString))
        ]


displayScheme :: Scheme -> String
displayScheme (Forall [] t) = displayType t
displayScheme (Forall vars t) = "∀" ++ concatMap (\(TV x) -> unpack x ++ " ") vars ++ ":" ++ displayType t

displayConstraint :: Constraint -> String
displayConstraint (t1, t2) = displayType t1  ++ "~~" ++ displayType t2

displayConstraints :: [Constraint] -> String
displayConstraints = (++"]") . foldl (\s c -> s ++ displayConstraint c ++ ",\n") "["

displaySubst :: Subst -> String
displaySubst env = "[" ++ Map.foldrWithKey (\(TV name) tp acc -> printf "%s -> %s, \n%s" name (displayType tp) acc) "]" env

displayTypeMap :: Map.Map Text Scheme -> String
displayTypeMap env =
    "[" ++
    Map.foldrWithKey
        (\name tp acc -> printf "%s : %s, \n%s" name (displayScheme tp) acc) "]" env


newtype InferState = InferState Int
type Infer a = StateT InferState (Except TypeError) a

type Subst = Map.Map TVar Type
type Constraint = (Type, Type)

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1
-- composeSubst s1 s2 = merge
--         (mapMaybeMissing passthrough)
--         (mapMaybeMissing passthrough)
--         (zipWithMaybeMatched (thing s1 s2))
--         s1
--         s2
--     where
--         passthrough _ v = Just v
--         thing s1 s2 k v1 v2 = Just (apply s2 v1)



class Substitutable a where
    apply :: Subst -> a -> a
    ftv :: a -> Set.Set TVar

instance Substitutable Type where
    apply s (TVar n) = case Map.lookup n s of
        Nothing -> TVar n
        Just t  -> t
    apply s (TCon c) = TCon c
    apply s (TApp t1 t2) = TApp (apply s t1) (apply s t2)

    ftv (TVar n) = Set.singleton n
    ftv (TCon _) = Set.empty
    ftv (TApp t1 t2) = ftv t1 `Set.union` ftv t2


instance Substitutable Scheme where
    ftv (Forall vars t) = ftv t `Set.difference` Set.fromList vars
    apply s (Forall vars t) = Forall vars (apply s' t) where
        s' = foldr Map.delete s vars


instance Substitutable a => Substitutable [a] where
    ftv xs = foldr (Set.union . ftv) Set.empty xs
    apply = map . apply


instance Substitutable a => Substitutable  (LetBindingF a) where
    apply s (Simple pat e) = Simple pat (apply s e)
    apply s (Rec pats) = Rec $ map (apply s) pats

    ftv (Simple pat e ) = ftv e
    ftv (Rec pats) = foldr (Set.union . ftv) Set.empty pats


instance Substitutable (AnnotatedProg Type) where
    apply s (AnnotatedProg datas lets) = AnnotatedProg datas (map (apply s) lets)
    ftv (AnnotatedProg datas lets) = foldr (Set.union . ftv) Set.empty lets



instance Substitutable TypeEnv where
    apply s TypeEnv {tenv, ctrs} = TypeEnv  (Map.map (apply s) tenv) ctrs
    ftv TypeEnv { tenv, ctrs } = ftv $ Map.elems tenv

instance Substitutable (Map.Map Text Scheme) where
    apply s tenv = Map.map (apply s) tenv
    ftv tenv = ftv $ Map.elems tenv


instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
    apply s (a, b) = (apply s a, apply s b)
    ftv (a, b) = Set.union (ftv a) (ftv b)

instance Substitutable (Cofree ExprF Type) where
    apply = fmap . apply
    -- Cofree ExprF SV
    ftv = foldMap ftv

remove :: TypeEnv -> Text -> TypeEnv
remove TypeEnv { tenv, ctrs } var = TypeEnv (Map.delete var tenv) ctrs

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall vars t where
    vars = Set.toList (ftv t `Set.difference` ftv env)


instantiate :: Scheme -> Infer Type
instantiate (Forall vars t) = do
    nvars <- mapM (const fresh) vars
    let sub = Map.fromList $ zip vars nvars
    return $ apply sub t


runInfer :: Infer a -> Except TypeError a
runInfer inf = x where
    x = evalStateT inf initState
    initState = InferState 0

tryTE :: Either TypeError a -> Infer a
tryTE (Right a) = return a
tryTE (Left err) = lift $ throwE err

fresh :: Infer Type
fresh = do
    (InferState nonce) <- S.get
    S.put $ InferState (nonce + 1)
    return $ TVar $ TV $ T.pack ("a" ++ show nonce)


lookupEnv :: Text -> TypeEnv -> Infer Type
lookupEnv v TypeEnv{tenv, ctrs}= do
    -- let res = Map.lookup v (traceMsgWith displayTypeMap ("lookup of " ++ T.unpack v) tenv)
    let res = Map.lookup v tenv
    case res of
        Just s -> instantiate s
        Nothing -> lift $ throwE (UnboundVariable v)


litType :: Lit -> Type
litType (Str _    ) = tString
litType (Num _    ) = tNum
litType (Boolean _) = tBoolean
litType Unit        = tUnit

mgu :: Type -> Type -> Infer Subst
mgu (TCon c) (TCon c') | c == c' = return nullSubst
mgu (TVar n) t = varBind n t
mgu t (TVar n) = varBind n t
mgu (TApp t1 t2) (TApp t1' t2') = solve [(t1, t1'), (t2, t2')]
mgu t1 t2 = lift $ throwE $ UnificationFail t1 t2

varBind :: TVar -> Type -> Infer Subst
varBind u t | t == TVar u          = return nullSubst
            | u `Set.member` ftv t = lift $ throwE $ InfiniteType u t
            | otherwise            = return $ Map.singleton u t

solve :: [Constraint] -> Infer Subst
solve [] = return nullSubst
solve ((t1, t2):cs) = do
    s <- mgu t1 t2
    s2 <- solve (apply s cs)
    return $ s2 `composeSubst` s


ti :: TypeEnv -> Expr -> Infer (Subst, AnnotatedExpr Type)
ti env = inner env . unwrap . coerceAnnotation where
    inner :: TypeEnv -> ExprF (Cofree ExprF ()) -> Infer (Subst, Cofree ExprF Type)

    inner env (Var name) = do
        t <- lookupEnv name env
        return (nullSubst, t :< Var name)

    inner env@TypeEnv{tenv, ctrs} (Call e1 e2) = do
        -- (s1, sub1@(ty1 :< tr1)) <- inner env (unwrap e1)
        (s1, sub1@(ty1 :< tr1)) <- inner env (unwrap e1)
        (s2, sub2@(ty2 :< tr2)) <- inner env (unwrap e2)
        tv <- fresh
        s3 <- mgu (apply s2 ty1) (tArr ty2 tv)
        let tree = apply s3 <$> Call sub1 sub2
        let
            ty1' = apply s2 ty1
            ty2' = apply nullSubst ty2
            s4   = s3 `composeSubst` s2 `composeSubst` s1
            tres = apply (trace ("got substitution: " ++ displaySubst s4) s4) tv
        return (s4, trace ("Inside call: " ++
                           displayTypeMap (apply s4 tenv) ++
                           " and result type is " ++
                           displayType tres ++
                           " and type of e1 is " ++
                           displayType ty1 ++
                           " and type of e2 is " ++
                           displayType ty2) tres:< tree)

    inner env (Let (PVar x@"t") e1 e2) = do
        (s1, binder@(t1 :< _)) <- inner env (unwrap e1)
        let t1'  = generalize (apply s1 env) (trace ("before " ++ displayType t1) t1)
        -- let t1'  = generalize (apply s1 env) t1
            env' = extend (trace ("current subst: " ++ displaySubst s1)env) (x, trace ("after " ++ displayScheme t1') t1')
            env'' = apply s1 env'
        (s2, res@(t2 :< _)) <- inner env'' (unwrap e2)
        let s3 = traceMsgWith displaySubst "let subst 3" (traceMsgWith displaySubst "let subst 1" s1 `composeSubst` traceMsgWith displaySubst "let subst 2" s2)
        return (s3, apply s3 t2 :< Let (PVar x) binder res)
        -- return (s3, (apply s3 (trace ("rsetype" ++ displayType t2) t2)) :< Let (PVar x) binder res)

    inner env (Let (PVar x) e1 e2) = do
        (s1, binder@(t1 :< _)) <- inner env (unwrap e1)
        let t1'  = generalize (apply s1 env) t1
            env' = extend env (x, t1')
        (s2, res@(t2 :< _)) <- inner env' (unwrap e2)
        let s3 = s1 `composeSubst` s2
        return (s3, (apply s3 t2) :< Let (PVar x) binder res)

    inner env (Let PNull e1 e2)= do
        (s1, binder) <- inner env $ unwrap e1
        (s2, res@(rt :< _)) <- inner env $ unwrap e2
        let sres = s1 `composeSubst` s2
            tres = apply sres rt
        return (sres, tres :< Let PNull binder res)

    inner env (Lambda (PVar x) e) = do
        tv <- fresh
        let env' = extend env (x, Forall [] tv)
        (s1, res@(t1 :< _)) <- inner env' $ unwrap e
        return (s1, (apply s1 (tv `tArr` t1)) :< Lambda (PVar x) res)

    inner env (Cond ec et ef) = do
        (s1, trcond@(tc :< _))  <- inner env $ unwrap ec
        (s2, trtrue@(tt :< _))  <- inner env $ unwrap et
        (s3, trfalse@(tf :< _)) <- inner env $ unwrap ef

        s4 <- solve [(tc, tBoolean), (tt, tf)]
        let s' = foldl1 composeSubst
                    (trace ("True branch: " ++
                            displayType tt  ++
                            "\nFalse branch : " ++
                            displayType tf) (reverse [s1, (traceMsgWith displaySubst "TrueSubstitution" s2), (traceMsgWith displaySubst "False Substitution" s3), traceMsgWith displaySubst "solved subst" s4]))
        let tree = apply s' <$> Cond trcond trtrue trfalse
        return ((traceMsgWith displaySubst "final cond subst" s'), apply s' tt :< tree)

    inner env (LFix e) = do
        tres <- fresh
        (s1, trlam@(ltype :< _)) <- inner env $ unwrap e
        s2 <- mgu (trace ("\n\n==========================\nfix subtype: " ++ displayType ltype) ltype) (tres `tArr` tres)
        -- s2 <- mgu ltype (tres `tArr` tres)
        let s3 = s1 `composeSubst` s2
        return (s3, apply s3 <$> ((apply s3 tres) :< LFix trlam))



    inner env@TypeEnv{tenv, ctrs} (Switch disc arms) = do
        (s, discres@(dtype :< _)) <- inner env $ unwrap disc
        patscheme <- getPatScheme (trace ("env in switch " ++ displayTypeMap tenv) env) (map fst arms)
        pattype <- instantiate (trace ("disc pattype: " ++ displayScheme patscheme ++
                                       " disc realtype " ++ displayType dtype) patscheme)
        tres <- fresh
        let armLabels = map fst arms
        (s', typedArms) <- foldrM (\(n, arm) (s, arms) -> do
            (s', tpd) <- inner (apply s env) $ unwrap arm
            return (s `composeSubst` s', apply s' tpd : arms)
            ) (s, []) arms
        let typedArms' = apply s' typedArms
        let armTypes = map getType typedArms'
            cs = (apply s' dtype, pattype) : map ((,) tres) (trace ("arm Types " ++ (show ( map displayType armTypes))) armTypes)
        s'' <- solve (trace ("Constraints: " ++ displayConstraints cs) (seq (trace ("arm labels: " ++ show armLabels) ()) cs))
        let sres = s' `composeSubst` s''
        let substArms = map (fmap $ apply sres) typedArms'
        let tres' = apply sres tres
        return (sres, trace ("switch returns with " ++ displayType tres') tres' :< Switch discres (zip armLabels substArms))

        where
            getType (t :< _) = t
            getPatScheme :: TypeEnv -> [Text] -> Infer Scheme
            getPatScheme env labels = do
                sgs <- mapM (getConScheme env) labels
                case sgs of
                    []  -> lift $ throwE $ MiscError "Empty matches not allowed, this should be unreachable"
                    h:t -> if all (schemeEq h) sgs then return h
                        else let offender = find (not . schemeEq h) sgs in
                            lift $ throwE $ MiscError $
                                "Con mismatch on " `T.append` (T.pack $ show labels) `T.append` ": " `T.append` T.pack (show h) `T.append` " vs " `T.append` (T.pack $ show $ fromJust offender)

            getConScheme :: TypeEnv -> Text -> Infer Scheme
            getConScheme TypeEnv{tenv, ctrs} cname =
                case Map.lookup cname tenv of
                    Just (Forall vars t) -> return $ Forall vars (getRetType t)
                    Nothing -> lift $ throwE $ MiscError ("Unknown constructor: " `T.append` cname)

            getRetType (TApp (TApp (TCon c) t2) t) | c == cArrow = getRetType t
            -- getRetType (TScheme (Forall vars t)) = TScheme (Forall vars (getRetType t))
            getRetType t = t

            schemeEq :: Scheme -> Scheme -> Bool
            schemeEq sgm1 sgm2 =
                case runExcept $ runInfer inf of
                    Left  _ -> False
                    Right _ -> True
                where
                    inf = do
                        t1 <- instantiate sgm1
                        t2 <- instantiate sgm2
                        solve [(t1, t2)]
            echoScheme :: [Text] -> Scheme -> Scheme
            echoScheme names sch = trace (show names ++ " " ++ show sch) sch




    inner env (Const lit) = return (nullSubst, litType lit :< Const lit)
    inner _ _ = undefined

inferLetDecl :: TypeEnv -> LetBindingF Expr -> Infer (Subst, TypeEnv)
inferLetDecl env l@(Simple _ _) = inferLetDecls env [l]
inferLetDecl env (Rec ls) = inferLetDecls env ls

inferLetDecls :: TypeEnv -> [LetBindingF Expr]  -> Infer (Subst, TypeEnv)
inferLetDecls TypeEnv{tenv, ctrs} lets = do
    tvars <- mapM (const fresh) lets
    let ext = map (\(Simple (PVar n) _, tv) -> (n, Forall [] tv)) $ zip lets tvars
    let env' = TypeEnv (Map.fromList ext `Map.union` tenv) ctrs
        generalizationEnvs = map (\(Simple (PVar n) _) -> remove env' n) lets
    results <- mapM
        (\case
            (Simple (PVar n) e) -> do
                (s, t :< _) <- ti env' e
                return (s, apply s t)
            (Simple PNull  _) -> error "Null patterns broken right now"
            (Simple (PCon _ _)   _) -> error "No destructurisation, sorry"
            (Rec _) -> error "unreachable"
        )
        lets

    let s = foldr1 composeSubst (map fst results)
        cs = map (second snd) (zip tvars results)



    solved <- solve (apply s cs)
    let s' = s `composeSubst` solved
        solvedTs = map (apply s' . snd) results
        env'' = apply s' env'
        genedTs = case lets of
            [Simple(PVar "foldr") _] -> let TypeEnv{tenv=tenv''} = head generalizationEnvs in trace (displayTypeMap  tenv'' ++ "\n" ++ (displayType $ head solvedTs)) $ map (uncurry generalize) $ zip generalizationEnvs solvedTs
            _ -> map (uncurry generalize) $ zip generalizationEnvs solvedTs
        resExt = [(n, t) | (Simple (PVar n) _, t) <- zip lets genedTs]
        resEnv = TypeEnv (Map.fromList resExt `Map.union` tenv) ctrs
    return (s', apply s' resEnv)



addDataDeclaration :: DataDecl -> TypeEnv -> TypeEnv
addDataDeclaration dd@(DataDecl con tp constrs) TypeEnv {tenv, ctrs} = TypeEnv env''' cs' where
    cs'    = con : ctrs
    env'   = foldl (\e cd@(ConDecl name ts) ->
                Map.insert name (conType dd cd) e) tenv constrs
    env''  = foldl (\e cd@(ConDecl name ts) ->
                Map.insert ("is" `T.append` name) (conPred dd cd) e) env' constrs
    env''' = foldl (\e cd@(ConDecl name ts) ->
                Map.fromList (conGetters dd cd) `Map.union` e) env'' constrs

inferProg :: Prog -> Infer ([DataDecl], TypeEnv)
inferProg (Prog datas lets) = do

    (constrs, datadecls) <- tryTE $
         foldrM
            (\d (cs, res) -> do
                dd@(DataDecl c tp cds) <- evalDataDeclaration cs d
                return (c:cs, dd:res))
            (builtinConstrs, [])
            datas

    let env = foldr addDataDeclaration startingEnv datadecls
    (s', env') <- foldlM addLetDec (nullSubst, env) lets
    return (datadecls, env')

    where
        addLetDec :: (Subst, TypeEnv) -> LetBindingF Expr ->  Infer (Subst, TypeEnv)
        addLetDec (s, env) l  = do
            (s', env') <- inferLetDecl env l
            return (s `composeSubst` s', env')

typeProg :: Prog -> Except TypeError ([DataDecl], [LetBindingF (AnnotatedExpr ())], TypeEnv)
typeProg p@(Prog datas lets) = do
    (evaledDatas, env) <- runInfer $ inferProg p
    let resLets = map (fmap coerceAnnotation) lets
    return (evaledDatas, resLets, (trace (displayTypeMap (tenv env)) env))


