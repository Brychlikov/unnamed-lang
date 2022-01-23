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
import Types
import Types.Infer
import Control.Monad.Trans.Except

import Text.Pretty.Simple
import Data.Function (on)
import Control.Comonad.Cofree (Cofree((:<)))


data Value
    = Number Float
    | Str    Text
    | Callable Clbl
    | Boolean Bool
    | Unit
    | Tuple (Value, Value)
    deriving (Eq, Show)

data Clbl
    = Builtin (Value -> IO Value)
    | Clojure C.Pattern Env (Env2 Value)
    | Fixpoint Text (Env2 Value)

instance Eq Clbl where
    f == g = False

instance Show Clbl where
    show (Builtin _) = "<builtin>"
    show Clojure {} = "<clojure>"
    show (Fixpoint name f) = "<fixpoint of " ++ T.unpack name ++ ">"


addValues :: Value -> Value -> Value
addValues (Number x) (Number y) = Number $ x + y
addValues (Str s) (Str s') = Str $ T.append s s'
addValues v1 v2 = error ("Can't add values: " ++ show v1 ++ " and " ++ show v2)

multValues :: Value -> Value -> Value
multValues (Number x) (Number y) = Number $ x * y
multValues _ _ = error "type error"

subValues :: Value -> Value -> Value
subValues (Number x) (Number y) = Number $ x - y
subValues _ _ = error "type error"


divValues :: Value -> Value -> Value
divValues (Number x) (Number y) = Number $ x / y
divValues _ _ = error "type error"

tupleValues :: Value -> Value -> Value
tupleValues v1 v2 = Tuple (v1, v2)

magicValues :: Value -> Value
magicValues v = error "No magic allowed, sorry"

eqValues :: Value -> Value -> Value
eqValues v1 v2 = Boolean $ v1 == v2

neqValues :: Value -> Value -> Value
neqValues v1 v2 = Boolean $ v1 /= v2

fstValue :: Value -> Value 
fstValue (Tuple (x, y)) = x
fstValue _ = error "type error"

sndValue :: Value -> Value 
sndValue (Tuple (x, y)) = y
sndValue _ = error "type error"

embed1 :: (Value -> Value) -> Value
embed1 f = Callable $ Builtin (return . f)

embed :: (Value -> Value -> Value) -> Value
embed f = Callable $ Builtin (\x -> return $ Callable $ Builtin (return . f x))

negValue :: Value -> Value
negValue (Number x) = Number $ -x
negValue _ = error "type error"


type Env = Map.Map Text Value
type Errortype = String


type Env2 = ReaderT (Map.Map Text Value) (ExceptT Errortype IO)

call :: Env2 Value -> Env2 Value -> Env2 Value
call m1 m2= do
    v1 <- m1
    case v1 of
        Callable (Builtin g) -> m2 >>= (liftIO . g)
        Callable (Clojure pat env body) ->
            m2 >>= (\v2 ->local (const $ Map.union (fromJust $ destructure pat v2) env) body)
        c@(Callable (Fixpoint name f)) ->
            local (Map.insert name c) (call f m2)
        _ -> lift $ throwE "sadly, you called a non-callable :("


interpret :: A.Expr -> Env2 Value
interpret = foldFix eval where

    eval :: A.ExprF (Env2 Value) -> Env2 Value
    eval (A.Const (C.Num x)) = return $ Number x
    eval (A.Const (C.Str s)) = return $ Str s
    eval (A.Const (C.Boolean b)) = return $ Boolean b
    eval (A.Var name)        = asks (Map.lookup name) >>= help
        where help (Just v) = return v
              help Nothing  = lift $ throwE $ "Unbound variable: " ++ show name
    eval (A.Let pat m1 m2)   = do
        v1 <- m1
        let addBound = Map.union (fromJust $ destructure pat v1)
        local addBound m2

    eval (A.Call m1 m2) = call m1 m2


    eval (A.Lambda pat m) = do
        env <- ask
        return $ Callable $ Clojure pat env  m

    eval (A.LFix name a) = return $ Callable $ Fixpoint name a

    eval (A.Cond mb mt mf) = mb >>= (\x -> if isTruthy x then mt else mf)


isTruthy :: Value -> Bool
isTruthy (Boolean b) = b
isTruthy _ = False

destructure :: C.Pattern -> Value -> Maybe Env
destructure (C.PVar name) v = Just $ Map.singleton name v

printValue :: Value -> IO Value
printValue v = do
    print v
    return Unit

initEnv :: Env
initEnv = Map.fromList
    [ ("print", Callable $ Builtin printValue)
    , ("(+)",    embed addValues)
    , ("(-)",    embed subValues)
    , ("(*)",    embed multValues)
    , ("(/)",    embed divValues)
    , ("(,)",    embed tupleValues)
    , ("(==)",   embed eqValues)
    , ("(!=)",   embed neqValues)
    , ("magic", embed1 magicValues)
    , ("fst",   embed1 fstValue)
    , ("snd",   embed1 sndValue)
    ]

unwrap :: Show a => Either a b -> b
unwrap = either (\a -> error ("Unwrap on Left value: " ++ show a)) id



run :: Text -> IO Value
run s = do
    let e = lower $ unwrap $ runParser Parser.pExpr "typedRepl" s
    case runExcept $ typeExpr e of
        Left err -> print err >> return Unit
        Right (t :< _) -> do
            res <- runExceptT $ flip runReaderT initEnv $ interpret e
            case res of
                Right v -> return v
                Left err -> putStrLn err >> return Unit

