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
    describe "Conditionals" $ do 
        it "supports basic conditionals" $ 
            run "if true then 20 else 30" `shouldReturn` Number 20
        it "runs if branches lazily" $ 
            run "if false then unknown_function 20 else 10" `shouldReturn` Number 10