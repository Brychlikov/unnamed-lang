{-# LANGUAGE OverloadedStrings #-}
module TypesSpec where
import Types 
import Test.Hspec
import Parser
import Data.Text (Text)
import qualified Ast.Common as C
import Text.Megaparsec (runParser)

cList :: Constructor
cList = Constructor  "List"    (KType `KArr` KType)

constrs :: [Constructor]
constrs = [ Constructor  "Number"  KType 
         , Constructor  "Boolean" KType 
         , cList]

tShouldSucceedOn :: Text -> Type -> Expectation
tShouldSucceedOn src ex = 
    case evalType constrs $ pTypeUnwrap src of 
        Left err  -> expectationFailure (show err)
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



