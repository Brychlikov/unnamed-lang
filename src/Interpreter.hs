{-# LANGUAGE FlexibleInstances #-}
module Interpreter where

import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Ast.Normal as A
import qualified Parser
import qualified Ast.Common as C
import Text.Megaparsec (runParser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Data.Fix
import Text.ParserCombinators.ReadP (many1)
import Ast.Lower (lower)


data Value
    = Number Float
    | Str    Text
    | Callable Clbl
    | Unit
    deriving (Eq, Show)

data Clbl
    = Builtin (Value -> IO Value)
    | Clojure C.Pattern Env (Env2 Value)

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

embed :: (Value -> Value -> Value) -> Value
embed f = Callable $ Builtin (\x -> return $ Callable $ Builtin (return . f x))

negValue :: Value -> Value
negValue (Number x) = Number $ -x
negValue _ = error "type error"


type Env = Map.Map Text Value


type Env2 = ReaderT (Map.Map Text Value) IO

interpret :: A.Expr -> Env2 Value
interpret = foldFix eval where

    eval :: A.ExprF (Env2 Value) -> Env2 Value
    eval (A.Const (C.Num x)) = return $ Number x
    eval (A.Const (C.Str s)) = return $ Str s
    eval (A.Var name)        = asks (fromJust . Map.lookup name)
    eval (A.Let pat m1 m2)   = do
        v1 <- m1
        let addBound = Map.union (fromJust $ destructure pat v1)
        local addBound m2
    eval (A.Call m1 m2) = do
        v1 <- m1
        v2 <- m2
        case v1 of
            Callable (Builtin g) -> liftIO $ g v2
            Callable (Clojure pat env body) ->
                local (const $ Map.union (fromJust $ destructure pat v2) env) body
            _ -> error "called a non-callable"


    eval (A.Lambda pat m) = do
        env <- ask
        return $ Callable $ Clojure pat env  m



-- interpret :: Env -> A.Expr -> IO Value

-- interpret _env (A.Const (C.Num x)) = return $ Number x
-- interpret _env (A.Const (C.Str s)) = return $ Str s

-- interpret env (A.Var name) = return $ env ! name
-- interpret env (A.Call e1 e2) = do
--     v1 <- interpret env e1
--     v2 <-  interpret env e2
--     case v1 of
--         Callable (Builtin g) -> g v2
--         Callable (Clojure pat closedEnv body) -> do
--             let toAdd = fromJust $ destructure pat v2
--             let env' = Map.union toAdd closedEnv
--             interpret env' body
--         _ -> error "called on non-callable"

-- interpret env (A.Lambda [pat] body) = return $ Callable $ Clojure pat env body
-- interpret env (A.Lambda (p:pats) body) = return $ Callable $ Clojure p env (A.Lambda pats body)


-- interpret env (A.Let (A.Simple pat e1) e2) = do
--     v1 <- interpret env e1
--     let toAdd = fromJust $ destructure pat v1
--     let env' = Map.union toAdd env
--     interpret env' e2

-- interpret env (A.Let (A.FunBinding name [arg] body) e2) = interpret env' e2 where 
--     env' = Map.insert name clo env
--     clo = Callable $ Clojure arg env body

-- interpret env (A.Let (A.FunBinding name (a:args) body) e2) = interpret env' e2 where 
--     env' = Map.insert name clo env
--     clo = Callable $ Clojure a env (A.Lambda args body)

-- interpret env _ = undefined


destructure :: C.Pattern -> Value -> Maybe Env
destructure (C.PVar name) v = Just $ Map.singleton name v

printValue :: Value -> IO Value
printValue v = do
    print v
    return Unit

initEnv :: Env
initEnv = Map.fromList
    [ ("print", Callable $ Builtin printValue) 
    , ("(+)",   embed addValues)
    , ("(-)",   embed subValues)
    , ("(*)",   embed multValues)
    , ("(/)",   embed divValues)
    ]

unwrap :: Show a => Either a b -> b
unwrap = either (\a -> error ("Unwrap on Left value: " ++ show a)) id

run :: Text -> IO Value
run = flip runReaderT initEnv . interpret . lower . unwrap . runParser Parser.pExpr "repl"

