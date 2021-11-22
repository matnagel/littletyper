module ParseExpression
  ( parseToExpression,
    ErrInfo,
  )
where

import Control.Applicative
import Control.Monad
import qualified Data.Map.Strict as Map
import Text.Trifecta

import Types

protectedKeywords = ["lambda"]

tokenize :: Parser a -> Parser a
tokenize p = p <* whiteSpace

pAtom :: Parser Expression
pAtom = CAtom . MkAtom <$> (char '\'' >> some letter)

pIdentifier :: Parser String
pIdentifier = do
  string <- (:) <$> letter <*> many alphaNum
  guard $ notElem string protectedKeywords
  return string

pVariable :: Parser Expression
pVariable = EVar <$> pIdentifier

lambdaEntry = void (symbolic 'λ') <|> void (symbol "lambda")

pLambda :: Parser Expression
pLambda =
  lambdaEntry >> do
    variables <- parens (some $ tokenize pIdentifier)
    expr <- braces pCompositeExpression
    return $ foldr CLambda expr variables

asum :: [Parser a] -> Parser a
asum = foldr (<|>) empty

pElementaryExpression :: Parser Expression
pElementaryExpression = tokenize $ asum ps
  where
    ps = [pLambda, pAtom, pVariable]

pType :: Parser Type
pType =
  parens pType <|> do
    typ <- symbol "Atom" >> return Atom
    f <- optional $ symbol "->" >> pType
    return $ maybe typ (Arrow typ) f

theAnnotated :: Parser Expression -> Parser Expression
theAnnotated p = do
  exp <- p
  annotation <- optional (symbolic ':' >> pType)
  return $ maybe exp (Athe exp) annotation

pCompositeExpression :: Parser Expression
pCompositeExpression =
  theAnnotated $
    foldr1 EApplication
      <$> some (pElementaryExpression <|> parens pCompositeExpression)

parseToExpression :: String -> Either ErrInfo Expression
parseToExpression str = case parseString (pCompositeExpression <* symbolic ';') mempty str of
  Success x -> Right x
  Failure err -> Left err
