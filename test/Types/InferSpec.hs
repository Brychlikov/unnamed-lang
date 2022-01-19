{-# LANGUAGE OverloadedStrings #-}
module Types.InferSpec where
import Types.Infer 
import Test.Hspec
import Interpreter

spec :: Spec
spec = do 
    it "types a simple polymorphic function (NOT RELIABLE) " $ 
        runWithType "let f x y = x in 10" `shouldReturn`  Number 10
