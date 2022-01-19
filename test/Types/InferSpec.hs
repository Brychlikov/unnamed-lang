{-# LANGUAGE OverloadedStrings #-}
module Types.InferSpec where
import Types.Infer
import Test.Hspec
import Interpreter
import Types
import Control.Monad.Trans.Except (runExcept)
import Parser (pExpr)
import Text.Megaparsec (runParser)
import Data.Text (Text)
import Control.Comonad.Cofree (Cofree((:<)))
import Ast.Lower (lower)


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



spec :: Spec
spec = do
    it "can test for type equality" $ do
        "2" `shouldType` TNum
        "2 + 2" `shouldType` TNum
        shouldNotType "2 + true"
        "fun x -> x + 1" `shouldType` TArr TNum TNum

    it "types simple let expressions" $ do
        "let x = 10 in x" `shouldType` TNum
        "let x = 10 in let y = x + x in y" `shouldType` TNum
        shouldNotType "let x = true in x + x"

    it "types function bindning let expressions" $ do
        "let f x y = x + y in f" `shouldType` TArr TNum TNum
        "let f x y z = x + y in let y = f 0 0 0 in f 0 0 true" `shouldType` TNum
        "let f x = x in f" `shouldType`
            TScheme (Forall [TV "x"] (TArr (TVar $ TV "x") (TVar $ TV "x")))
    