{-# LANGUAGE OverloadedStrings #-}
module Utils.UniqSpec where

import Test.Hspec
import Utils.Uniq
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity
import Data.Text (Text)

spec :: Spec 
spec = do 
    describe "Uniq" $ do 
        it "generates increasing names" $ do 
            let m = sequence [unique "test" | _ <- [0..5]]
            (evalState (runUniqT m) Map.empty) `shouldBe`
                [ "test"
                , "test1"
                , "test2"
                , "test3"
                , "test4"
                , "test5"
                ]
        it "has independent counters" $ do 
            let m = do 
                    n1 <- unique "foo"
                    n2 <- unique "bar"
                    n3 <- unique "foo"
                    return (n1, n2, n3)
            evalState (runUniqT m) Map.empty `shouldBe` ("foo", "bar", "foo1")