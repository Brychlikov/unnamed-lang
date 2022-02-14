{-# LANGUAGE OverloadedStrings #-}
module CompilerSpec where

import Compiler
import Test.Hspec
import Interpreter (runInterpreterCatchOut)
import Data.Text
import qualified Data.Text as T
import Debug.Trace (traceShowId)

eImplementationsAgreeon :: Text -> Expectation
eImplementationsAgreeon t =
    cout `shouldReturn` iout where
    cout = runCompiledExpr t
    iout = runInterpreterCatchOut t

shouldPrint :: Text -> Text -> Expectation
shouldPrint src ex = do 
    res <-  runCompiledStd src 
    case res of 
        Left err -> expectationFailure err 
        Right out -> T.strip (traceShowId out) `shouldBe` ex


spec :: Spec
spec = do
    -- Old tests, Interpreter is now too out of sync
    return ()


    -- it "tests something" $ do
    --     eImplementationsAgreeon "let x = 10 in print x"
    -- it "supports functions" $ do
    --     eImplementationsAgreeon "let f x y = x + y in print (f 10 20)"
    --     eImplementationsAgreeon "let f x y z = x + y in print (f 10 20 false)"
    -- it "supports tail recursion" $ do
    --     eImplementationsAgreeon
    --         "let cnt acc n = if n == 0 then acc else cnt (acc + 1) (n-1) in print (cnt 0 100000)"
    -- it "supports pairs" $ do
    --     eImplementationsAgreeon "let p = (10, false) in print(snd p)"
    -- it "works with large numbers" $ do
    --     eImplementationsAgreeon "print (100000 * 20398)"

-- WATCH OUT FOR PITFALL
-- Shelly automatically adds newlines at the end of output if there is none
-- I drop it in shouldPrint
    -- describe "Compiler implementation" $ do
    --     it "can test output" $ do
    --         "let _ = print 20" `shouldPrint` "20"
    --         "let _ = println 20" `shouldPrint` "20"

        -- context "lists" $ do 
        --     it "has lists in prelude" $ do 
        --         "let _ = print (length [1, 2, 3])" `shouldPrint` "3"
        --     it "has map in prelude" $ do 
        --         "let adds x y = x + y \ 
        --         \let _ = print ((map (adds 1) [1, 2, 3]) == [2, 3, 4])" `shouldPrint` "true"

        --     it "has find in prelude" $ do 
        --         "let equal x y = x == y \ 
        --         \let _ = match (find (equal 10) [1, 3, 1, 2, 10, 53, 1]) with \ 
        --         \| Some v -> print v end" `shouldPrint` "10"
            
    