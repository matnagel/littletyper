module Parser.Expression
  ( pDefinition,
    pExpression,
    pType,
  )
where

import Control.Applicative
  ( Alternative (empty, some, (<|>)),
    optional,
  )
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Parser.Token
  ( tokenAtom,
    tokenIdentifier,
    tokenVariable,
  )
import Text.Trifecta
  ( Parser,
    Result (Failure, Success),
    braces,
    parens,
    parseString,
    symbol,
    symbolic,
  )
import Types (Expression (AThe, CLambda, EApplication), Type (..))

lambdaEntry :: Parser ()
lambdaEntry = void (symbolic 'λ') <|> void (symbol "lambda")

pLambda :: Parser Expression
pLambda =
  lambdaEntry >> do
    variables <- parens (some tokenIdentifier)
    expr <- braces pCompositeExpression
    return $ foldr CLambda expr variables

asum :: [Parser a] -> Parser a
asum = foldr (<|>) empty

pElementaryExpression :: Parser Expression
pElementaryExpression = asum ps
  where
    ps = [pLambda, tokenAtom, tokenVariable]

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
  return $ maybe exp (AThe exp) annotation

pCompositeExpression :: Parser Expression
pCompositeExpression =
  theAnnotated $
    foldr1 EApplication
      <$> some (pElementaryExpression <|> parens pCompositeExpression)

pExpression :: Parser Expression
pExpression = pCompositeExpression

pDefinition :: Parser (String, Type, Expression)
pDefinition = do
  symbol "const"
  name <- tokenIdentifier
  symbolic ':'
  t <- pType
  symbolic '='
  exp <- pExpression
  return (name, t, exp)
