{-# LANGUAGE OverloadedStrings #-}
module CompilerSpec where 

import Compiler
import Test.Hspec
import Interpreter (runInterpreterCatchOut)
import Compiler (runCompiled)
import Data.Text

implementationsAgreeOn :: Text -> Expectation
implementationsAgreeOn t = 
    cout `shouldReturn` iout where 
    cout = runCompiled t
    iout = runInterpreterCatchOut t


spec :: Spec 
spec = do
    it "tests something" $ do 
        implementationsAgreeOn "let x = 10 in print x"
    it "supports functions" $ do
        implementationsAgreeOn "let f x y = x + y in print (f 10 20)"
        implementationsAgreeOn "let f x y z = x + y in print (f 10 20 false)"
    it "supports tail recursion" $ do
        implementationsAgreeOn 
            "let cnt acc n = if n == 0 then acc else cnt (acc + 1) (n-1) in print (cnt 0 100000)"
    it "supports pairs" $ do 
        implementationsAgreeOn "let p = (10, false) in print(snd p)"
    it "works with large numbers" $ do 
        implementationsAgreeOn "print (100000 * 20398)"
    