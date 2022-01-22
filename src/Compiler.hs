module Compiler where

import Ast.Normal
import Types 
import Types.Infer 
import Control.Monad.Trans.Writer
import Control.Comonad.Identity (Identity)
import qualified Data.Text as T
import Control.Comonad.Cofree (Cofree((:<)))
import Ast.Common
import qualified Data.Map as Map


type Comp = WriterT T.Text Identity

emptyComp :: Comp () 
emptyComp = tell "const STD = require('./std.js');"

emitLit :: Lit -> Comp ()
emitLit = undefined

nameToJs :: T.Text -> T.Text 
nameToJs name = Map.findWithDefault name name dict where 
    dict = Map.fromList 
        [ ("(+)", "add") 
        , ("(-)", "sub")
        , ("(*)", "mult")
        , ("(/)", "div")
        , ("(,)", "pair")
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

compile :: TypedExpr -> Comp ()
compile (t :< Const lit) = emitLit lit
compile (t :< Var  name) = tell $ nameToJs name
compile (t :< (Call e1 e2)) = do 
    surroundParens $ compile e1 
    surroundParens $ compile e2
compile _ = undefined