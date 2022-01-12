{-# LANGUAGE FlexibleInstances #-}
module Interpreter where

import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Ast as A
import qualified Parser
import Text.Megaparsec (runParser)
import Data.Text (Text)
import qualified Data.Text as T


data Value
    = Number Float
    | Str    Text
    | Callable Clbl
    | Unit
    deriving (Eq, Show)

data Clbl
    = Builtin (Value -> IO Value)
    | Clojure A.Pattern Env A.Expr

instance Eq Clbl where
    f == g = False

instance Show Clbl where
    show (Builtin _) = "<builtin>"
    show Clojure {} = "<clojure>"


addValues :: Value -> Value -> Value
addValues (Number x) (Number y) = Number $ x + y
addValues (Str s) (Str s') = Str $ T.append s s'
addValues _ _ = error "type error"

multValues :: Value -> Value -> Value
multValues (Number x) (Number y) = Number $ x * y
multValues _ _ = error "type error"

subValues :: Value -> Value -> Value
subValues (Number x) (Number y) = Number $ x - y
subValues _ _ = error "type error"


divValues :: Value -> Value -> Value
divValues (Number x) (Number y) = Number $ x / y
divValues _ _ = error "type error"

negValue :: Value -> Value
negValue (Number x) = Number $ -x
negValue _ = error "type error"


type Env = Map.Map Text Value


interpret :: Env -> A.Expr -> IO Value

interpret _env (A.Const (A.Num x)) = return $ Number x
interpret _env (A.Const (A.Str s)) = return $ Str s

interpret env (A.Var name) = return $ env ! name
interpret env (A.Call e1 e2) = do
    v1 <- interpret env e1
    v2 <-  interpret env e2
    case v1 of
        Callable (Builtin g) -> g v2
        Callable (Clojure pat closedEnv body) -> do
            let toAdd = fromJust $ destructure pat v2
            let env' = Map.union toAdd closedEnv
            interpret env' body
        _ -> error "called on non-callable"

interpret env (A.Binop op lhs rhs) = do
    lv <- interpret env lhs
    rv <- interpret env rhs
    return $ op_to_func op lv rv where

    op_to_func A.Plus  = addValues
    op_to_func A.Minus = subValues
    op_to_func A.Mult  = multValues
    op_to_func A.Div   = divValues

interpret env (A.Lambda [pat] body) = return $ Callable $ Clojure pat env body
interpret env (A.Lambda (p:pats) body) = return $ Callable $ Clojure p env (A.Lambda pats body)

interpret env (A.Neg e) = interpret env e >>= (return . negValue)

interpret env (A.Let (A.Simple pat e1) e2) = do
    v1 <- interpret env e1
    let toAdd = fromJust $ destructure pat v1
    let env' = Map.union toAdd env
    interpret env' e2

interpret env (A.Let (A.FunBinding name [arg] body) e2) = interpret env' e2 where 
    env' = Map.insert name clo env
    clo = Callable $ Clojure arg env body

interpret env (A.Let (A.FunBinding name (a:args) body) e2) = interpret env' e2 where 
    env' = Map.insert name clo env
    clo = Callable $ Clojure a env (A.Lambda args body)

interpret env _ = undefined


destructure :: A.Pattern -> Value -> Maybe Env
destructure (A.PVar name) v = Just $ Map.singleton name v

printValue :: Value -> IO Value
printValue v = do
    print v
    return Unit

initEnv :: Env
initEnv = Map.fromList
    [ ("print", Callable $ Builtin printValue) ]

unwrap :: Show a => Either a b -> b
unwrap = either (\a -> error ("Unwrap on Left value: " ++ show a)) id

run :: Text -> IO Value
run = interpret initEnv . unwrap . runParser Parser.pExpr "repl"

