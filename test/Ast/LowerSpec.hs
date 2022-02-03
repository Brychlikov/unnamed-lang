{-# LANGUAGE OverloadedStrings #-}

module Ast.LowerSpec where
import Test.Hspec
import Test.Hspec.Megaparsec
import Ast.Lower
import Ast.Normal
import Ast.Common
import Data.Text (Text)
import qualified Data.Text as T

import Text.Megaparsec
import Parser


listDecl :: Text
listDecl = "data List a \n\ 
           \= Empty \n\ 
           \| Cons a (List a)\n"

spec :: Spec
spec = do
    it "lowers a match on a list" $ do
        eLowersTo
            "match Cons 1 Empty with\n\ 
            \| Empty -> 0 \n\
            \| Cons a b -> a \n\ 
            \end"
            (elet (PVar "_matchVar") (ecall (ecall (evar "Cons") (econst $ Num 1)) (evar "Empty"))
                  (eswitch (evar "_matchVar")
                    [ ("Empty", econst $ Num 0)
                    , ("Cons", elet (PVar "a") (ecall (evar "getCons0") (evar "_matchVar"))
                                (elet (PVar "b") (ecall (evar "getCons1") (evar "_matchVar")) (evar "a")))
                    ]))


eLowersTo :: Text -> Expr -> Expectation
eLowersTo s ast = parse (lower <$> pExpr) "" s `shouldParse` ast


