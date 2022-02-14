module Compiler where

import Ast.Normal
import Types
-- import Types.Infer
import Types.NewInfer
import Control.Monad.Trans.Writer
import Control.Comonad.Identity (Identity, runIdentity)
import qualified Data.Text as T
import Control.Comonad.Cofree (Cofree((:<)), ComonadCofree (unwrap))
import qualified Ast.Common as C
import qualified Data.Map as Map
import Text.Megaparsec (runParser, parseErrorPretty, errorBundlePretty)
import qualified Parser
import Ast.Lower (lower, lowerProg)
import Control.Monad.Trans.Except (runExcept)
import Prelude hiding (readFile)
import System.IO hiding (readFile)
import Data.Text.IO (readFile)
import Shelly ( print_stdout, run, setStdin, shelly )
import Data.Bifunctor (Bifunctor(bimap, first, second))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty((:|)) )
import Debug.Trace (traceShowId)


data Tail = Tail | NonTail deriving Show

data ProcessedProg = ProcessedProg
    { decls        :: [DataDecl]
    , lets         :: [LetBindingF (AnnotatedExpr (Tail, ()))]
    }

process :: Prog -> Either TypeError ProcessedProg
process prog@(Prog datas lets) = do
    (decls, noTailCallLets, env) <- runExcept $ typeProg prog
    let lets = map (fmap markTailCalls) noTailCallLets
    return $ ProcessedProg decls (seq env lets)


markTailCalls :: Show a => AnnotatedExpr a -> AnnotatedExpr (Tail, a)
markTailCalls = aux NonTail where
    aux tail (t :< (Const l)) = (NonTail, t) :< Const l
    aux tail (t :< (Var v)) = (NonTail, t) :< Var v
    -- you may not like it, but this is what peak performance looks like
    aux tail (t :< (Call (t1 :< Call (t11 :< Var "(;)") e1) e2)) 
           = (tail, t) :< Call ((NonTail, t1) :< Call ((NonTail, t11) :< Var "(;)") (aux NonTail e1)) (aux tail e2)
    aux tail (t :< (Call e1 e2)) = (tail, t) :< Call (aux NonTail e1) (aux NonTail e2)
    -- TODO: I miss some tail calls here, eg
    -- let x = f 10 in x
    aux tail (t :< (Let pat e1 e2)) = (tail, t) :< Let pat (aux NonTail e1) (aux tail e2)
    aux tail (t :< (LFix e)) = (tail, t) :< LFix (aux NonTail e)
    aux tail (t :< (Lambda pat e)) = (tail, t) :< Lambda pat (aux Tail e)
    aux tail (t :< (Cond ec et ef)) = (tail, t) :< Cond (aux NonTail ec) (aux tail et) (aux tail ef)
    aux tail (t :< (Switch e arms)) = (tail, t) :< Switch (aux NonTail e) (map (second (aux tail)) arms)

type Comp = WriterT T.Text Identity

emptyComp :: Comp ()
emptyComp = tell "const STD = require('./std.js');"

emitLit :: C.Lit -> Comp ()
emitLit (C.Str t) = surround "\"" "\"" (tell t)
emitLit (C.Num x) = tell $ T.pack $ show x
emitLit (C.Boolean True) = tell "true"
emitLit (C.Boolean False) = tell "false"
emitLit C.Unit = tell "unit";

nameToJs :: T.Text -> T.Text
nameToJs name = Map.findWithDefault name name dict where
    dict = Map.fromList
        [ ("(+)", "add")
        , ("(-)", "sub")
        , ("(*)", "mult")
        , ("(/)", "div")
        , ("(,)", "pair")
        , ("(==)", "eq")
        , ("(!=)", "neq")
        , ("(;)", "seq")
        , ("(++)", "strConcat")
        ]

surround :: T.Text -> T.Text ->  Comp a -> Comp a
surround open close m = do
    tell open
    res <- m
    tell close
    return res

surroundParens :: Comp a -> Comp a
surroundParens = surround "(" ")"

surroundBraces :: Comp a -> Comp a
surroundBraces = surround "{" "}"

surroundBrackets :: Comp a -> Comp a
surroundBrackets = surround "[" "]"

surroundQuotes :: Comp a -> Comp a 
surroundQuotes = surround "\"" "\""


stringLiteral :: T.Text -> Comp ()
stringLiteral lit = surround "\"" "\"" (tell lit)

compile :: AnnotatedExpr (Tail, ()) -> Comp ()
compile (t :< Const lit) = emitLit lit
compile (t :< Var  name) = tell $ nameToJs name
compile ((Tail, t) :< (Call e1 e2)) = do
    tell "jump("
    compile e1
    tell ","
    compile e2
    tell ")"
compile ((NonTail, t) :< (Call e1 e2)) = do
    tell "trampoline("
    compile e1
    tell ","
    compile e2
    tell ")"
compile (t :< (Let (C.PVar name) e1 e2)) = do
    tell "(function(){let "
    tell name
    tell "="
    compile e1
    tell "; return "
    compile e2
    tell ";})()"
    -- tell "(function("
    -- tell name
    -- tell "){return "
    -- compile e2 
    -- tell ";})"
    -- surroundParens $ compile e1



compile (t :< LFix e) = 
    case e of 
        (_ :< Lambda _ e2) -> compile e2
        _ -> error "unreachable"

compile (t :< Lambda (C.PVar name) e) = do
    tell "function("
    tell name
    tell ")"
    surroundBraces $ do
        tell "return "
        compile e
        tell ";"
compile (t :< Cond ec et ef) = do
    surroundParens (compile ec)
    tell " ? "
    compile et
    tell " : "
    compile ef


compile (t :< Switch e arms) = do 
    let last = tell "(function(){throw new Error('match arms non-exhaustive');})()"
    let eComp = compile e
    foldr (\(name, armE) next -> 
        emitIfStatement 
            (emitAccessJSField "conTag" eComp >> tell "==" >> (surroundQuotes $ tell name))
            (compile armE)
            next) last arms

compile _ = error "GIMME A BREAK"

emitIfStatement :: Comp () -> Comp () -> Comp () -> Comp() 
emitIfStatement ec et ef = do 
    surroundParens ec 
    tell " ? "
    surroundParens et 
    tell " : "
    surroundParens ef
    

emitAccessJSField :: T.Text -> Comp () -> Comp () 
emitAccessJSField f c = do 
    c 
    tell "."
    tell f

emitJSField :: T.Text -> Comp () -> Comp ()
emitJSField field value = do
    tell field
    tell ": "
    value
    tell ","


emitInstanceObj :: T.Text -> [Comp ()] -> Comp ()
emitInstanceObj name children = do
    tell "{"
    emitJSField "conTag" (stringLiteral name)
    emitJSField "children" (surroundBrackets $ sequence_ [c >> tell "," | c <- children])
    tell "}"

separateBy :: T.Text -> [Comp ()] -> Comp ()
separateBy sep args = sequence_ [c >> tell sep | c <- args]

nameOfTail :: Tail -> T.Text
nameOfTail Tail = "jump"
nameOfTail NonTail = "trampoline"

emitJSFunctionCall :: Tail -> Comp () -> Comp () -> Comp ()
emitJSFunctionCall tailStatus func arg = do
    tell (nameOfTail tailStatus)
    func
    tell ","
    arg
    tell ")"


coerceNonEmpty :: [a] -> NonEmpty a
coerceNonEmpty (h:t) = h :| t
coerceNonEmpty []    = error "coerceNoneEmpty called on empty list"

emitJSFunctionDef :: Maybe T.Text -> [Comp ()] -> Comp () -> Comp ()
emitJSFunctionDef fname args body = inner fname (coerceNonEmpty args) body where

    inner :: Maybe T.Text -> NonEmpty (Comp ()) -> Comp () -> Comp()

    inner fname (a:|[]) body = do
        tell "function "
        maybe (return ()) tell fname
        surroundParens a
        surroundBraces $ surround "return " ";" body

    inner fname (a :| (a' : args)) body = do
        tell "function "
        maybe (return ()) tell fname
        surroundParens a
        surroundBraces $ surround "return " ";" $ inner Nothing (a' :| args) body


makeVarComps :: T.Text -> Int -> [Comp ()]
makeVarComps t n = inner t n [] where
    inner t 0 acc = acc
    inner t n acc = inner t (n-1) ((tell t >> tell (T.pack $ show n )) : acc)

compileDataDecl :: DataDecl -> Comp ()
compileDataDecl (DataDecl con tp condecls) = do

    -- constructors
    separateBy "\n" $ map compileConDecl condecls

    -- predicates
    separateBy "\n" $
        [ emitJSFunctionDef
            (Just ("is" `T.append` coname))
            [tell "x"]
            (tell "x.conTag == " >> stringLiteral coname)
        | (ConDecl coname ts) <- condecls ]

    -- selectors
    separateBy "\n" $
        [ emitJSFunctionDef
            (Just ("get" `T.append` coname `T.append` T.pack (show n)))
            [tell "x"]
            (tell "x.children" >> surroundBrackets (tell $ T.pack $ show n))
        | (ConDecl coname ts) <- condecls,
          (n, _) <- zip [0..] ts]

    where
        vars coname ts = makeVarComps coname (length ts)

        compileConDecl :: ConDecl -> Comp () 
        compileConDecl (ConDecl coname []) = do 
            tell "let "
            tell coname 
            tell "= "
            emitInstanceObj coname []
            tell ";"

        compileConDecl (ConDecl coname ts) = 
            emitJSFunctionDef 
                (Just coname)
                (vars coname ts) 
                (emitInstanceObj coname (vars coname ts)) 


compileLetDecl :: LetBindingF (AnnotatedExpr (Tail, ())) -> Comp ()
compileLetDecl (Simple (C.PVar name) e) = do
    tell "let "
    tell name
    tell "= "
    compile e
    tell ";\n"

compileLetDecl (Simple (C.PNull) e) = do 
    compile e 
    tell ";"

compileLetDecl (Rec lets) = do 
    mapM_ compileLetDecl lets

compileLetDecl (Simple (C.PCon _ _) e) = error "Destructuring didn't cut it"



compileProcessed :: ProcessedProg -> Comp ()
compileProcessed (ProcessedProg datas lets) = do
    mapM_ compileDataDecl datas
    mapM_ compileLetDecl lets

compileSrc :: T.Text -> Either String T.Text
compileSrc s = do
    p <- first errorBundlePretty $ runParser Parser.pExpr "src" s
    let tree = lower p
    bimap show 
          (snd . runIdentity . runWriterT . compile . markTailCalls . fmap (const ())) 
          $ runExcept (typeExpr tree)

compileProgSrc :: T.Text -> Either String T.Text 
compileProgSrc s = do 
    p <- first errorBundlePretty $ runParser Parser.pProg "src" s
    let prog = lowerProg p
    procProg <- first show $ process prog
    return $ snd $ runIdentity $ runWriterT $ compileProcessed procProg

compileProgWithStd :: T.Text -> IO (Either String T.Text)
compileProgWithStd s = do 
    langStd <- readFile "std.lang"
    let res = compileProgSrc (langStd `T.append` s) 
    case res of 
        Left err -> return $ Left err 
        Right code -> do 
            prelude <- readFile "std.js"
            return $ Right $ prelude `T.append` code

runJSCode :: T.Text -> IO T.Text
runJSCode t = shelly $ print_stdout False $  do
    setStdin t
    run "node" []

runCompiledExpr :: T.Text -> IO T.Text
runCompiledExpr s = do
    prelude <- readFile "std.js"
    case compileSrc s of
        Left err -> return $ T.pack err
        Right js -> runJSCode (prelude `T.append` js)


runCompiledStd :: T.Text -> IO (Either String T.Text)
runCompiledStd s = do 
    c <- compileProgWithStd s
    case c of 
        Left err -> return $ Left err
        Right js -> Right <$> runJSCode js