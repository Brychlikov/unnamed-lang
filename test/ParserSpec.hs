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

tParsesTo :: Text -> Type -> Expectation
tParsesTo s t = parse pType "" s `shouldParse` t

dParsesTo :: Text -> Decl  -> Expectation
dParsesTo s d = parse pDecl "" s `shouldParse` d

spec :: Spec
spec = do
    describe "Expression parser" $ do
        it "parses float arithmethic" $
            "2.0 + 2.0" `eParsesTo` Binop Plus (Const $ Num 2.0) (Const $ Num 2.0)

        it "parses int arithmethic" $
            "2 + 2 * 2" `eParsesTo` Binop Plus (Const $ Num 2) (Binop Mult (Const $ Num 2) (Const $ Num 2))

        it "parses boolean operators" $ do 
            "1 == 0" `eParsesTo` Binop EqEq (Const $ Num 1) (Const $ Num 0)
            "1 != 0" `eParsesTo` Binop Neq (Const $ Num 1) (Const $ Num 0)

        it "parses boolean operators with correct precedence" $ do 
            "1 + 1 == 2" `eParsesTo` 
                Binop EqEq 
                    (Binop Plus (Const $ Num 1) (Const $ Num 1))
                    (Const $ Num 2)
            "0, 1 == 2" `eParsesTo`
                Binop Pair 
                    (Const $ Num 0) 
                    (Binop EqEq (Const $ Num 1) (Const $ Num 2))


        it "parses variables with underscore" $ do
            "under_score" `eParsesTo` Var "under_score"
            "_x" `eParsesTo` Var "_x"

        it "parses single let expressions" $
            "let x = 20 in x" `eParsesTo`
            Let (Simple (PVar "x") (Const $ Num 20)) (Var "x")

        it "rejects let binders starting with capital letter" $
            parse pExpr "" `shouldFailOn` "let Bad = 20 in Bad"

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

        it "parses boolean constants" $ do
            "true" `eParsesTo` Const (Boolean True)
            "false" `eParsesTo` Const (Boolean False)

        it "parses if expressions" $ do
            "if true then 10 else 20" `eParsesTo`
                Cond (Const $ Boolean True) (Const $ Num 10) (Const $ Num 20)
            "if foo (2 + 2) false then bar x y z else 20" `eParsesTo`
                Cond
                    (Call
                        (Call (Var "foo") (Binop Plus (Const $ Num 2) (Const $ Num 2)))
                        (Const $ Boolean False))
                    (Call (Call (Call (Var "bar") (Var "x")) (Var "y")) (Var "z"))
                    (Const $ Num 20)

        it "parses string literals" $
            "\"krowa\"" `eParsesTo` Const (Str "krowa")

        it "parses (some) escapes" $ 
            "\"\\nkrowa\\t\"" `eParsesTo` Const (Str "\nkrowa\t")

        it "parses pairs" $ do 
            "2, 2" `eParsesTo` Binop Pair (Const $ Num 2) (Const $ Num 2)
            "1 + 2, 3, 4" `eParsesTo`
                Binop Pair 
                    (Binop Plus (Const $ Num 1) (Const $ Num 2))
                    (Binop Pair (Const $ Num 3) 
                                (Const $ Num 4))

    describe "Declaration parser" $ do
        it "parses types" $ do 
            "Int" `tParsesTo` Con "Int"
            "Boolean" `tParsesTo` Con "Boolean"
            "a" `tParsesTo` TVar "a"
        it "parses type applications" $ do 
            "List a" `tParsesTo` App (Con "List") (TVar "a")
            "Either a Int" `tParsesTo` App (App (Con "Either") (TVar "a")) (Con "Int")
            "List (List Boolean)" `tParsesTo` App (Con "List") (App (Con "List") (Con "Boolean"))
        it "parses arrow types" $ do 
            "a -> b" `tParsesTo` App (App (Con "(->)") (TVar "a")) (TVar "b")
            "a -> b -> c" `tParsesTo` App (App (Con "(->)") (TVar "a")) 
                                          (App (App (Con "(->)") (TVar "b")) (TVar "c"))
        it "parses a data declaration" $ do 
            "data List a = Empty | Cons a (List a)" `dParsesTo` 
                DDecl (DataDecl "List" ["a"] 
                        [ConDecl "Empty" [], ConDecl "Cons" [TVar "a", App (Con "List") (TVar "a")]])
        it "parses let declarations" $ do 
            "let x = 10" `dParsesTo` LDecl (Simple (PVar "x") (Const $ Num 10))
            "let f x y = x y" `dParsesTo` 
                LDecl (FunBinding "f" [PVar "x", PVar "y"] (Call (Var "x") (Var "y")))

