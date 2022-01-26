{-# LANGUAGE OverloadedStrings #-}
module InterpreterSpec where 

import Interpreter 
import Test.Hspec


spec :: Spec 
spec = do
    it "runs simple arithmehtic expressions" $ 
        run "2 + 2" `shouldReturn` Number 4
    describe "Let expressions" $ do
        it "supports basic bindings" $ 
            run "let x = 10 in x" `shouldReturn` Number 10
        it "allows variable shadowing" $ 
            run "let x = 10 in let x = 20 in x" `shouldReturn` Number 20
        it "runs function definitions" $ do
            run "let f x = x + 10 in f 10" `shouldReturn` Number 20
            run "let g x y = x - y in g 10 5" `shouldReturn` Number 5
    describe "Functions" $ do 
        it "allows curried functions" $ do
            run "let f x y = x + y in let g = f 10 in g 5 + g 1" `shouldReturn` Number 26
        it "supports lambdas" $ do 
            run "(fun x -> x + 10) 10" `shouldReturn` Number 20
        it "supports multi argument lambdas" $ do 
            run "(fun x y z -> x * y * z) 2 3 4" `shouldReturn` Number 24
        it "supports first-class functions" $ do 
            run "let call_inc f x = f (x+1) in call_inc (fun x -> x) 1" `shouldReturn` Number 2

        it "supports recursive functions" $ do 
            run "let sum n = if n == 0 then 0 else n + sum (n-1) in sum 5" `shouldReturn` Number 15
            run "let fib n = if n == 0 then 0 else  (if n == 1 then 1 else fib (n-1) + fib (n-2)) in fib 10"
                `shouldReturn` Number 55
    describe "Conditionals" $ do 
        it "supports basic conditionals" $ 
            run "if true then 20 else 30" `shouldReturn` Number 20
        it "runs if branches lazily" $ 
            run "if false then magic 20 else 10" `shouldReturn` Number 10
    describe "Strings" $ do 
        it "supports string literals" $
            run "\"krowa\"" `shouldReturn` Str "krowa"

    describe "Pairs" $ do
        it "supports pair construction" $ 
            run "2, 2" `shouldReturn` Constructee "(,)" [Number 2, Number 2]

    describe "Equality" $ do 
        it "supports basic equality" $ do
            run "2 == 2" `shouldReturn` Boolean True
            run "1 == 2" `shouldReturn` Boolean False

        it "supports structural equality on pairs" $ do
            run "(1, 2) == (1, 2)" `shouldReturn` Boolean True
            run "(1, 2) != (1, 2)" `shouldReturn` Boolean False
            run "(1, (1, 2)) != (1, (1, 3))" `shouldReturn` Boolean True