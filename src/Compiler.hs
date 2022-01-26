module Compiler where

import Ast.Normal
import Types
import Types.Infer
import Control.Monad.Trans.Writer
import Control.Comonad.Identity (Identity, runIdentity)
import qualified Data.Text as T
import Control.Comonad.Cofree (Cofree((:<)), ComonadCofree (unwrap))
import Ast.Common
import qualified Data.Map as Map
import Text.Megaparsec (runParser, parseErrorPretty, errorBundlePretty)
import qualified Parser
import Ast.Lower (lower)
import Control.Monad.Trans.Except (runExcept)
import Prelude hiding (readFile)
import System.IO hiding (readFile)
import Data.Text.IO (readFile)
import Shelly ( print_stdout, run, setStdin, shelly )
import Data.Bifunctor (Bifunctor(bimap, first))



markTailCalls :: TypedExpr -> AnnotatedExpr (Bool, Type)
markTailCalls = aux False where
    aux tail (t :< (Const l)) = (False, t) :< Const l
    aux tail (t :< (Var v)) = (False, t) :< Var v
    aux tail (t :< (Call e1 e2)) = (tail, t) :< Call (aux False e1) (aux False e2)
    -- TODO: I miss some tail calls here, eg
    -- let x = f 10 in x
    aux tail (t :< (Let pat e1 e2)) = (tail, t) :< Let pat (aux False e1) (aux False e2)
    aux tail (t :< (LFix n e)) = (tail, t) :< LFix n (aux False e)
    aux tail (t :< (Lambda pat e)) = (tail, t) :< Lambda pat (aux True e)
    aux tail (t :< (Cond ec et ef)) = (tail, t) :< Cond (aux False ec) (aux tail et) (aux tail ef)

type Comp = WriterT T.Text Identity

emptyComp :: Comp ()
emptyComp = tell "const STD = require('./std.js');"

emitLit :: Lit -> Comp ()
emitLit (Str t) = surround "\"" "\"" (tell t)
emitLit (Num x) = tell $ T.pack $ show x
emitLit (Boolean True) = tell "true"
emitLit (Boolean False) = tell "false"

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

compile :: AnnotatedExpr (Bool, Type) -> Comp ()
compile (t :< Const lit) = emitLit lit
compile (t :< Var  name) = tell $ nameToJs name
compile ((True, t) :< (Call e1 e2)) = do
    tell "jump("
    compile e1
    tell ","
    compile e2
    tell ")"
compile ((False, t) :< (Call e1 e2)) = do
    tell "trampoline("
    compile e1
    tell ","
    compile e2
    tell ")"
compile (t :< (Let (PVar name) e1 e2)) = do 
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
    

    
compile (t :< LFix name e) = compile e
compile (t :< Lambda (PVar name) e) = do
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


compileSrc :: T.Text -> Either String T.Text
compileSrc s = do
    p <- first errorBundlePretty $ runParser Parser.pExpr "src" s
    let tree = lower p
    bimap show (snd . runIdentity . runWriterT . compile . markTailCalls) $ runExcept (typeExpr tree)

runJSCode :: T.Text -> IO T.Text
runJSCode t = shelly $ print_stdout False $  do 
    setStdin t
    run "node" []

runCompiled :: T.Text -> IO T.Text
runCompiled s = do 
    prelude <- readFile "std.js"
    case compileSrc s of
        Left err -> return $ T.pack err 
        Right js -> runJSCode (prelude `T.append` js)
    
