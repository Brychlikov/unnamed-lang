{-# LANGUAGE FlexibleInstances, GeneralisedNewtypeDeriving #-}
module Interpreter where

import Data.Map ((!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import qualified Ast.Normal as A
import qualified Parser
import qualified Ast.Common as C
import Text.Megaparsec (runParser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity

import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.Reader as RT (local, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class
import Data.Fix
import Text.ParserCombinators.ReadP (many1)
import Ast.Lower (lower)
import Types
import Types.Infer
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as ET (throwE, runExcept, runExceptT)

import Text.Pretty.Simple
import Data.Function (on)
import Control.Comonad.Cofree (Cofree((:<)))
import Control.Monad (liftM)
import Control.Monad.Trans.Writer (Writer)
import qualified Control.Monad.Trans.Writer as WT (tell, runWriter)
import Data.List (stripPrefix)
import Text.Printf


data Value m
    = Number Double
    | Str    Text
    | Callable (Clbl m)
    | Boolean Bool
    | Unit
    | Constructee Text [Value m]
    deriving (Eq, Show)

data Clbl m
    = Builtin (Value m -> m (Value m))
    | Clojure C.Pattern (Env m) (m (Value m))
    | Fixpoint (m (Value m))

instance Eq (Clbl m) where
    f == g = False

instance Show (Clbl m) where
    show (Builtin _) = "<builtin>"
    show Clojure {} = "<clojure>"
    show (Fixpoint f) = "<fixpoint>"


addValues :: Value m -> Value m -> Value m
addValues (Number x) (Number y) = Number $ x + y
addValues (Str s) (Str s') = Str $ T.append s s'
addValues v1 v2 = error ("Can't add values: " ++ show v1 ++ " and " ++ show v2)

multValues :: Value m -> Value m -> Value m
multValues (Number x) (Number y) = Number $ x * y
multValues _ _ = error "type error"

subValues :: Value m -> Value m -> Value m
subValues (Number x) (Number y) = Number $ x - y
subValues _ _ = error "type error"


divValues :: Value m -> Value m -> Value m
divValues (Number x) (Number y) = Number $ x / y
divValues _ _ = error "type error"

tupleValues :: Value m -> Value m -> Value m
tupleValues v1 v2 = Constructee "(,)" [v1, v2]

magicValues :: Value m -> Value m
magicValues v = error "No magic allowed, sorry"

eqValues :: Value m -> Value m -> Value m
eqValues v1 v2 = Boolean $ v1 == v2

neqValues :: Value m -> Value m -> Value m
neqValues v1 v2 = Boolean $ v1 /= v2

fstValue :: Value m -> Value  m
fstValue (Constructee "(,)" vs) = head vs
fstValue _ = error "type error"

sndValue :: Value m -> Value m 
sndValue (Constructee "(,)" vs) = head $ tail vs
sndValue _ = error "type error"

embed1 :: MonadInterpret m => (Value m -> Value m) -> Value m
embed1 f = Callable $ Builtin (return . f)

embed :: MonadInterpret m => (Value m -> Value m -> Value m) -> Value m
embed f = Callable $ Builtin (\x -> return $ Callable $ Builtin (return . f x))

negValue :: Value m -> Value m
negValue (Number x) = Number $ -x
negValue _ = error "type error"


type Env m = Map.Map Text (Value m)
type Errortype = String


class Monad m => MonadInterpret m where
    local  :: (Env m -> Env m) -> m v -> m v
    ask    :: m (Env m)
    tell   :: Text -> m ()
    throwE :: Errortype -> m a

    asks   :: (Env m -> a) -> m a
    asks f =  liftM f ask

newtype InterpretIO a = InterpretIO{ runInterpretIO :: ReaderT (Env InterpretIO) (ExceptT Errortype IO) a}
    deriving(Functor, Applicative, Monad)

instance MonadInterpret InterpretIO where 
    local  f = InterpretIO . RT.local f . runInterpretIO
    ask      = InterpretIO RT.ask 
    tell   t = InterpretIO (liftIO $ putStrLn $ T.unpack t)
    throwE e = InterpretIO (lift $ ET.throwE e)


newtype InterpretTextBuffer a = InterpretTextBuffer { 
    runInterpretTextBuffer :: ReaderT 
                              (Env InterpretTextBuffer) 
                              (ExceptT Errortype (Writer Text)) 
                              a
    }
    deriving (Functor, Applicative, Monad)

instance MonadInterpret InterpretTextBuffer where 
    local  f = InterpretTextBuffer . RT.local f . runInterpretTextBuffer
    ask      = InterpretTextBuffer RT.ask 
    tell   t = InterpretTextBuffer (lift $ lift $ WT.tell t)
    throwE e = InterpretTextBuffer (lift $ ET.throwE e)


call :: MonadInterpret m => m (Value m) -> m (Value m) -> m (Value m)
call m1 m2= do
    v1 <- m1
    case v1 of
        Callable (Builtin g) -> m2 >>= g
        Callable (Clojure pat env body) ->
            m2 >>= (\v2 ->local (const $ Map.union (fromJust $ destructure pat v2) env) body)
        c@(Callable (Fixpoint f)) -> 
            do 
                clbl <- f
                let (lam, name) = case clbl of
                        c2@(Callable (Clojure (C.PVar name) _ _))  -> (c2, name)
                        _ -> undefined
                local (Map.insert name c) (call f m2)
        _ -> throwE "sadly, you called a non-callable :("


interpret :: MonadInterpret m => A.Expr -> m (Value m)
interpret = foldFix eval where

    eval :: MonadInterpret m => A.ExprF (m (Value m)) -> m (Value m)
    eval (A.Const (C.Num x)) = return $ Number x
    eval (A.Const (C.Str s)) = return $ Str s
    eval (A.Const (C.Boolean b)) = return $ Boolean b
    eval (A.Const C.Unit) = return $ Unit
    eval (A.Var name)        = asks (Map.lookup name) >>= help
        where help :: MonadInterpret m => Maybe (Value m) -> m (Value m)
              help (Just v) = return v
              help Nothing  = throwE $ "Unbound variable: " ++ show name
    eval (A.Let pat m1 m2)   = do
        v1 <- m1
        let addBound = Map.union (fromJust $ destructure pat v1)
        local addBound m2

    eval (A.Call m1 m2) = call m1 m2


    eval (A.Lambda pat m) = do
        env <- ask
        return $ Callable $ Clojure pat env  m

    eval (A.LFix a) = return $ Callable $ Fixpoint a

    eval (A.Cond mb mt mf) = mb >>= (\x -> if isTruthy x then mt else mf)
    eval (A.Switch _ _) = error "no matches in interpreter, sorry"


isTruthy :: Value m -> Bool
isTruthy (Boolean b) = b
isTruthy _ = False

destructure :: C.Pattern -> Value m -> Maybe (Env m)
destructure (C.PVar name) v = Just $ Map.singleton name v

printValue :: MonadInterpret m => Value m -> m (Value m)
printValue v = do
    tell $ display v
    tell "\n"
    return Unit
    where 
    tryStripPrefix pre s = fromMaybe s (stripPrefix pre s)
    display (Number x)       = T.pack . reverse . tryStripPrefix "0." . reverse $ printf "%f" x
    display (Str t)          = t
    display (Boolean True)   = "true"
    display (Boolean False)  = "false"
    display Unit             = "()"
    display (Constructee "(,)" [v1, v2]) = "(" `T.append` display v1 `T.append` ", " `T.append` display v2 `T.append` ")"
    display v = T.pack $ show v

initEnv :: MonadInterpret m => Env m
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


runInterpreterCatchOut :: Text -> Text 
runInterpreterCatchOut s = do
    let e = lower $ unwrap $ runParser Parser.pExpr "typedRepl" s
    case ET.runExcept $ typeExpr e of
        Left err -> T.pack $ show err 
        Right (t :< _) -> let
            (res, buf) = WT.runWriter $ ET.runExceptT $ flip runReaderT initEnv $ runInterpretTextBuffer $ interpret e in
            case res of
                Right _ -> buf
                Left err -> T.pack $ show err

run :: Text -> IO (Value InterpretIO)
run s = do
    let e = lower $ unwrap $ runParser Parser.pExpr "typedRepl" s
    case ET.runExcept $ typeExpr e of
        Left err -> print err >> return Unit
        Right (t :< _) -> do
            res <- ET.runExceptT $ flip runReaderT initEnv $ runInterpretIO $ interpret e
            case res of
                Right v -> return v
                Left err -> putStrLn err >> return Unit

