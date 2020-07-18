module ParseExpression (
    parseToExpression,
    Parseable,
    ErrInfo,
    isType
    )
    where

import qualified Data.Map.Strict as Map

import Control.Applicative
import Control.Monad

import Types

import Text.Trifecta

class Parseable a where
    stdParser :: Parser a

instance Parseable Atom where
    stdParser = MkAtom <$> (char '\'' >> (some letter))

tokenize :: Parser a -> Parser a
tokenize p = p <* whiteSpace

pAtom :: Parseable a => Parser (Expression a)
pAtom = CAtom <$> stdParser

pIdentifier :: Parser String
pIdentifier = do
    string <- ((:) <$> letter <*> many alphaNum)
    guard (string /= "lambda")
    return string

pVariable :: Parser (Expression a)
pVariable = EVar <$> pIdentifier

pLambda :: Parseable a => Parser (Expression a)
pLambda =  intro >> do
    variables <- parens (some $ tokenize $ pIdentifier)
    expr <- braces $ pExpression
    return $ foldr applyLambda expr variables
    where applyLambda x y = CLambda x y
          intro = (symbolic 'λ' >> return ()) <|> (symbol "lambda" >> return ())

asum :: [Parser a] -> Parser a
asum ps = foldr (<|>) empty ps

pSimpleExpression :: Parseable a => Parser (Expression a)
pSimpleExpression = tokenize $ asum ps
    where ps = [pLambda, pAtom, pVariable]

pExpression :: Parseable a => Parser (Expression a)
pExpression = parens pExpression <|> foldr1 applyApplication <$> some pSimpleExpression
    where applyApplication x y = EApplication x y

parseToExpression :: Parseable a => String -> Either ErrInfo (Expression a)
parseToExpression str = case parseString (pExpression <* symbolic ';') mempty str of
    Success x -> Right x
    Failure err -> Left err

isType :: Expression Atom -> Type -> Bool
isType (CAtom _) Atom = True
isType (EVar "x") (TypeOfVariable "x") = True
isType _ _ = False
