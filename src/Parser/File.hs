module Parser.File
  ( parseToDefinition,
    parseFileContent,
  )
where

import Control.Applicative
  ( Alternative (empty, some, (<|>)),
    optional,
  )
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Parser.Expression (pExpression, pType)
import Parser.Token
  ( tokenAtom,
    tokenIdentifier,
    tokenVariable,
  )
import Text.Trifecta
  ( ErrInfo,
    Parser,
    Result (Failure, Success),
    braces,
    parens,
    parseString,
    symbol,
    symbolic,
  )
import Types (Expression (Athe, CLambda, EApplication), Type (..))

pDefinition :: Parser (String, Type, Expression)
pDefinition = do
  symbol "const"
  name <- tokenIdentifier
  symbolic ':'
  t <- pType
  symbolic '='
  exp <- pExpression
  return (name, t, exp)

parseToDefinition :: String -> Either ErrInfo (String, Type, Expression)
parseToDefinition str = case parseString (pDefinition <* symbolic ';') mempty str of
  Success x -> Right x
  Failure err -> Left err

parseFileContent :: String -> Either ErrInfo String
parseFileContent = Right
