module ParseExpression (
    parseToExpression
    )
    where

import qualified Data.Map.Strict as Map

import Control.Applicative
import Control.Monad

import Types

import Text.Trifecta

tokenize :: Parser a -> Parser a
tokenize p = p <* whiteSpace

pAtom :: Parser Expression
pAtom = char '\'' >> CAtom <$> some letter

pIdentifier :: Parser String
pIdentifier = do
    string <- ((:) <$> letter <*> many alphaNum)
    guard (string /= "lambda")
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
    where ps = [pAtom, pLambda, pVariable]

pExpression :: Parser Expression
pExpression = parens pExpression <|> foldr1 applyApplication <$> some pSimpleExpression
    where applyApplication x y = EApplication x y

parseToExpression :: String -> Either ErrInfo Expression
parseToExpression str = case parseString (pExpression <* symbolic ';') mempty str of
    Success x -> Right x
    Failure err -> Left err
