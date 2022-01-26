{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Text (Text, append, cons, pack, unpack)
import Data.Functor( ($>), (<$) )
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import qualified Data.Set as Set

import Ast.Full
import Ast.Common

type Parser = Parsec Void Text

keywords :: Set.Set Text
keywords = Set.fromList ["let", "in", "fun", "if", "then", "else"]

reserved :: Set.Set Text 
reserved = Set.fromList ["true", "false"]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#-" "-#")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

number :: Parser Double
number = choice [ try $ lexeme L.float
                , try $ lexeme L.decimal ]

stringLiteral :: Parser Text 
stringLiteral = pack <$> (char '"' >> manyTill L.charLiteral (char '"'))

pString :: Parser Expr 
pString = Const . Str <$> stringLiteral

pBoolean :: Parser Expr 
pBoolean = Const . Boolean <$> choice [True <$ symbol "true", False <$ symbol "false"]

pIdentifier :: Parser Text
pIdentifier = do
    s <- try inner 
    if Set.member s keywords then fail $ "keyword " ++ unpack s ++ " not allowed here" else return s 

    where
        collect a b c = cons a $ append b c

        legalChars :: Parser Char
        legalChars = alphaNumChar <|> char '_'

        legalStart :: Parser Char 
        legalStart = letterChar <|> char '_'
        inner = lexeme  (   collect
                        <$> legalStart
                        <*> (pack <$> many legalChars)
                        <*> (pack <$> many (char '\''))
                        <?> "variable"
                        )


pVariable :: Parser Expr
pVariable = Var <$> pIdentifier


pNumeric :: Parser Expr
pNumeric = Const . Num <$> number

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
    [ parens pExpr
    , pLetExpr
    , pIfExpr
    , pBoolean
    , pLambda
    , pVariable
    , pNumeric
    , pString
    ]

pOpExpr :: Parser Expr
pOpExpr = makeExprParser pTerm operatorTable where
    operatorTable =
      [ [ appl ]
      , [ prefix "-" Neg
        , prefix "+" id
        ]
      , [ binary "*" (Binop Mult)
        , binary "/" (Binop Div)
        ]
      , [ binary "+" (Binop Plus)
        , binary "-" (Binop Minus)
        ]
      , [ binary "==" (Binop EqEq)
        , binary "!=" (Binop Neq)
        ]
      , [ InfixR (Binop Pair <$ symbol ",")
        ]
      ]
    appl = InfixL (Call <$ space)
    space = sc *> notFollowedBy (choice . map symbol $ Set.toList keywords)

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)


pPattern :: Parser Pattern 
pPattern = PVar <$> pIdentifier

pLetBinding :: Parser LetBinding 
pLetBinding = choice 
  [ try $  Simple <$> pPattern <*> (symbol "=" *> pExpr)
  , try $ FunBinding <$> pIdentifier <*> many pPattern <*> (symbol "=" *> pExpr)]



pLetExpr :: Parser Expr
pLetExpr = Let 
  <$> (symbol "let" *> pLetBinding) 
  <*> (symbol "in"  *> pExpr)

pIfExpr :: Parser Expr 
pIfExpr = Cond 
  <$> (symbol "if" *> pExpr)
  <*> (symbol "then" *> pExpr)
  <*> (symbol "else" *> pExpr)


pLambda :: Parser Expr 
pLambda = Lambda 
  <$> (symbol "fun" *> some pPattern)
  <*> (symbol "->"  *> pExpr)
pExpr :: Parser Expr
pExpr = pOpExpr


prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

unwrap :: Show a => Either a b -> b 
unwrap (Left a) = error $ "unwrap called on left: " ++ show a
unwrap (Right b) = b

fullParse :: Text -> Expr 
fullParse = unwrap . runParser pExpr ""