module ParseExpression (
    parseToExpression,
    ErrInfo
    )
    where

import qualified Data.Map.Strict as Map

import Control.Applicative
import Control.Monad

import Types

import Text.Trifecta

protectedKeywords = ["lambda"]

tokenize :: Parser a -> Parser a
tokenize p = p <* whiteSpace

pAtom :: Parser Expression
pAtom = CAtom <$> MkAtom <$> (char '\'' >> (some letter))

pIdentifier :: Parser String
pIdentifier = do
    string <- ((:) <$> letter <*> many alphaNum)
    guard $ not $ elem string protectedKeywords
    return string

pVariable :: Parser Expression
pVariable = EVar <$> pIdentifier

pLambda :: Parser Expression
pLambda =  intro >> do
    variables <- parens (some $ tokenize $ pIdentifier)
    expr <- braces $ pExpression
    return $ foldr applyLambda expr variables
    where applyLambda x y = CLambda x y
          intro = (symbolic 'λ' >> return ()) <|> (symbol "lambda" >> return ())

asum :: [Parser a] -> Parser a
asum ps = foldr (<|>) empty ps

pSimpleExpression :: Parser Expression
pSimpleExpression = tokenize $ asum ps
    where ps = [pLambda, pAtom, pVariable]

pType :: Parser Type
pType = parens pType <|> do
    typ <- (symbol "Atom" >> return Atom)
    f <- optional $ (symbol "->") >> pType
    case f of
        Nothing -> return typ
        Just range -> return $ Arrow typ range

theAnnotated :: Parser Expression -> Parser Expression
theAnnotated p = do
    exp <- p
    annotation <- optional (symbolic ':' >> pType)
    case annotation of
        Nothing -> return exp
        Just typ -> return $ Athe exp typ

pExpression :: Parser Expression
pExpression = theAnnotated $ foldr1 applyApplication
        <$> some (pSimpleExpression <|> parens pExpression)
    where applyApplication x y = EApplication x y

parseToExpression :: String -> Either ErrInfo Expression
parseToExpression str = case parseString (pExpression <* symbolic ';') mempty str of
    Success x -> Right x
    Failure err -> Left err

parsePlay :: String -> Either ErrInfo Expression
parsePlay str = case parseString (pExpression <* symbolic ';') mempty str of
    Success x -> Right x
    Failure err -> Left err

