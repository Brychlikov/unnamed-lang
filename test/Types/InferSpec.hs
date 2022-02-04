{-# LANGUAGE OverloadedStrings #-}
module Types.InferSpec where
import Types.Infer
import Test.Hspec
import Interpreter
import Types
import Control.Monad.Trans.Except (runExcept)
import Parser (pExpr, parseProgUnwrap)
import Text.Megaparsec (runParser)
import Data.Text (Text)
import Control.Comonad.Cofree (Cofree((:<)))
import Ast.Lower (lower, lowerProg)


typeEq :: Type -> Type -> Expectation
typeEq t1 t2 = case runExcept $ runSolver [(t1, t2)] of
    Left err -> expectationFailure $ show err
    Right _  -> return ()

shouldType :: Text -> Type -> Expectation
shouldType src tp = do
    let Right expr         = lower <$> runParser pExpr "" src
        Right (resTp :< _) = runExcept $ typeExpr expr
    tp `typeEq` tp

shouldNotType :: Text -> Expectation
shouldNotType src = do
    let Right expr = lower <$> runParser pExpr "" src
    case runExcept $ typeExpr expr of
        Left _ -> return ()
        Right (resTp :< _) -> expectationFailure ("Expression should not type, but typed to " ++  show resTp)

progShouldType :: Text -> Expectation 
progShouldType src = do
    let hprog = parseProgUnwrap src 
    let lprog = lowerProg hprog
    case runExcept $ typeProg lprog of 
        Left err -> expectationFailure (show err)
        Right _ -> return ()


spec :: Spec
spec = do
    it "can test for type equality" $ do
        "2" `shouldType` tNum
        "2 + 2" `shouldType` tNum
        shouldNotType "2 + true"
        "fun x -> x + 1" `shouldType` tArr tNum tNum

    it "types simple let expressions" $ do
        "let x = 10 in x" `shouldType` tNum
        "let x = 10 in let y = x + x in y" `shouldType` tNum
        shouldNotType "let x = true in x + x"

    it "types function bindning let expressions" $ do
        "let f x y = x + y in f" `shouldType` tArr tNum tNum
        "let f x y z = x + y in let y = f 0 0 0 in f 0 0 true" `shouldType` tNum
        "let f x = x in f" `shouldType`
            TScheme (Forall [TV "x"] (tArr (TVar $ TV "x") (TVar $ TV "x")))

    it "types recursive functions" $ do
        "let sum n = if n == 0 then 0 else n + sum (n-1) in sum" `shouldType` tArr tNum tNum
        "let fib n = if n == 0 then 0 else (if n == 1 then 1 else (fib (n-1)) + fib (n-2) in fib"
            `shouldType` tArr tNum (tArr tNum tNum)
    it "types tuples" $ do
        "(1, 2)" `shouldType` ((TCon tup `TApp` tNum) `TApp` tNum)
        "let f x = (x, x) in f" `shouldType`
            TScheme (Forall [TV "a"]
                (TVar (TV "a") `tArr` (TCon tup `TApp` TVar (TV "a") `TApp` TVar (TV "a"))))

    it "types unit" $ 
        "()" `shouldType` tUnit
    
    it "types a simple program" $ do 
        progShouldType "let x = 42\ 
                       \let _ = print x" 

    it "types a program with mutually recursive functions" $ do 
        progShouldType "let even n = if n == 0 then true else odd (n-1)\ 
                       \let odd  n = if n == 0 then false else even(n-1)\ 
                       \let _ = print (odd 10)"
    
    it "types a program with data declarations" $ do 
        progShouldType "data List a \ 
                       \= Empty \
                       \| Cons a (List a)\
                       \let test = Cons 1 (Cons 2 Empty)"
        
    it "types a program with a match statement" $ do 
        progShouldType "data List a \ 
                       \= Empty \
                       \| Cons a (List a)\
                       \match Cons 1 Empty with \ 
                       \| Empty -> 0 \ 
                       \| Cons a b -> a \ 
                       \end"