{-# LANGUAGE OverloadedStrings #-}
module ParserSpec where 

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Data.Text ( Text ) 
import Parser
import Ast.Full
import Ast.Common

eParsesTo :: Text -> Expr -> Expectation
eParsesTo s ast = parse pExpr "" s `shouldParse` ast

spec :: Spec 
spec = describe "Expression parser" $ do 
    it "parses float arithmethic" $
        "2.0 + 2.0" `eParsesTo` Binop Plus (Const $ Num 2.0) (Const $ Num 2.0)

    it "parses int arithmethic" $ 
        "2 + 2 * 2" `eParsesTo` Binop Plus (Const $ Num 2) (Binop Mult (Const $ Num 2) (Const $ Num 2))

    it "parses variables with underscore" $ do
        "under_score" `eParsesTo` Var "under_score"
        "_x" `eParsesTo` Var "_x"
        
    it "parses single let expressions" $ 
        "let x = 20 in x" `eParsesTo` 
        Let (Simple (PVar "x") (Const $ Num 20)) (Var "x")

    it "parses left-nested let expressions" $
        "let x = let y = 10 in y in x" `eParsesTo`
        Let (Simple (PVar "x") (Let (Simple (PVar "y") (Const $ Num 10)) (Var "y")))
            (Var "x")

    it "parses right-nested let expressions" $
        "let x = 10 in let y = 20 in x + y" `eParsesTo` 
        Let (Simple (PVar "x") (Const $ Num 10)) 
            (Let (Simple (PVar "y") (Const $ Num 20)) (Binop Plus (Var "x") (Var "y")))

    it "parses calls" $ do
        "x y" `eParsesTo` Call (Var "x") (Var "y")
        "x y z" `eParsesTo` Call (Call (Var "x") (Var "y")) (Var "z")

    it "parses calls inside let-expressions" $ do 
        "let x = a b in x z" `eParsesTo`
            Let (Simple (PVar "x") (Call (Var "a") (Var "b"))) (Call (Var "x") (Var "z"))

    it "parses function definition" $
        "let f x = x in f 20" `eParsesTo`
        Let (FunBinding "f" [PVar "x"] (Var "x")) (Call (Var "f") (Const $ Num 20))

    it "parses lambdas" $ 
        "fun x -> x * x" `eParsesTo` Lambda [PVar "x"] (Binop Mult (Var "x") (Var "x"))

    it "parses multi argument lambdas" $ 
        "fun x y -> x + y" `eParsesTo`
        Lambda [PVar "x", PVar "y"] (Binop Plus (Var "x") (Var "y"))
