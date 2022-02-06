{-# LANGUAGE OverloadedStrings #-}
module TypesSpec where
import Types 
import Test.Hspec
import Parser
import Data.Text (Text)
import qualified Ast.Common as C
import Text.Megaparsec (runParser)


tShouldSucceedOn :: Text -> Type -> Expectation
tShouldSucceedOn src ex = 
    let tp = pTypeUnwrap src in
    case evalType builtinConstrs tp of 
        Left err  -> expectationFailure (show err ++ " on type " ++ show tp)
        Right res -> res `shouldBe` ex

pTypeUnwrap :: Text -> C.Type
pTypeUnwrap = unwrapParseError . runParser pType ""

spec :: Spec 
spec = do 
    describe "evalType" $ do 
        it "evals simple constructors" $ do 
            "Number" `tShouldSucceedOn` tNum
            "Boolean" `tShouldSucceedOn` tBoolean
        it "evals applied constructors" $ do 
            "List Number" `tShouldSucceedOn` (TApp (TCon cList) tNum)

        it "evals function types" $ do 
            "Number -> Number"   `tShouldSucceedOn` (tNum `tArr` tNum)
            "(Number -> Number)" `tShouldSucceedOn` (tNum `tArr` tNum)



