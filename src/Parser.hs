{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Data.Text (Text, append, cons, pack, unpack, singleton)
import qualified Data.Text as T (concat)
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
keywords = Set.fromList ["let", "in", "fun", "if", "then", "else", "data"]

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

pIdentWithFirst  :: Parser Text -> Parser Text
pIdentWithFirst p = do 
    res <- try inner 
    if Set.member res keywords 
      then fail $ "no keyword " ++ unpack res ++ " not allowed here"
    else 
      return res
    where 
      inner = lexeme $ do 
        s1 <- p
        s2 <- pack <$> many (alphaNumChar  <|> char '_')
        s3 <- pack <$> many (char '\'')
        return $ T.concat [s1, s2, s3]


pVarIdent :: Parser Text 
pVarIdent = pIdentWithFirst (singleton <$> (lowerChar <|> char '_'))

pTypeIdent :: Parser Text 
pTypeIdent  = pIdentWithFirst (singleton <$> upperChar)

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

prefix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

pPattern :: Parser Pattern 
pPattern = PVar <$> pVarIdent

pLetBinding :: Parser LetBinding 
pLetBinding = choice 
  [ try $  Simple <$> pPattern <*> (symbol "=" *> pExpr)
  , try $ FunBinding <$> pVarIdent <*> many pPattern <*> (symbol "=" *> pExpr)]



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


typeAtom :: Parser Type
typeAtom = choice 
  [ parens pType
  , Con <$> pTypeIdent
  , TVar <$> pVarIdent
  ]

pType :: Parser Type
pType = makeExprParser typeAtom operatorTable where
    operatorTable = 
      [ [ InfixL (App <$ space) 
        , InfixR (arrow <$ symbol "->")
        ]
      ]
    arrow t1 t2 = App (App (Con "(->)") t1) t2
    space = sc *> notFollowedBy (choice . map symbol $ Set.toList keywords)

pConDecl :: Parser ConDecl 
pConDecl = ConDecl <$> pTypeIdent <*> many (try typeAtom)

pDataDecl :: Parser DataDecl 
pDataDecl = do 
  symbol "data"
  name <- pTypeIdent 
  typeVars <- many pVarIdent
  symbol "="
  first <- pConDecl 
  rest <- many (symbol "|" *> pConDecl)
  return $ DataDecl name typeVars (first : rest)

pDecl :: Parser Decl 
pDecl = choice 
  [ DDecl <$> pDataDecl
  , LDecl <$> (symbol "let" *> pLetBinding)
  ]

pProg :: Parser Prog 
pProg = Prog <$> many pDecl 

unwrap :: Show a => Either a b -> b 
unwrap (Left a) = error $ "unwrap called on left: " ++ show a
unwrap (Right b) = b

unwrapParseError :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => 
                     Either (ParseErrorBundle s e) a  ->
                     a
unwrapParseError (Right a) = a 
unwrapParseError (Left err) = error $ errorBundlePretty err

fullParse :: Text -> Expr 
fullParse = unwrap . runParser pExpr ""

parseProgUnwrap :: Text -> Prog
parseProgUnwrap = unwrapParseError . runParser pProg ""