module Parser.File
  ( parseToDefinition,
    parseToExpression,
    parseFileContent,
    ErrInfo,
  )
where

import Control.Applicative
  ( Alternative (empty, some, (<|>)),
    optional,
  )
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Parser.Expression (pDefinition, pExpression, pType)
import Parser.Token
  ( tokenAtom,
    tokenIdentifier,
    tokenVariable,
  )
import Text.Trifecta
  ( ErrInfo (ErrInfo),
    Parser,
    Result (Failure, Success),
    braces,
    parens,
    parseString,
    symbol,
    symbolic,
  )
import Types (Expression (AThe, CLambda, EApplication), Type (..))

parseToDefinition :: String -> Either ErrInfo (String, Type, Expression)
parseToDefinition str = case parseString (pDefinition <* symbolic ';') mempty str of
  Success x -> Right x
  Failure err -> Left err

parseToExpression :: String -> Either ErrInfo Expression
parseToExpression str = case parseString (pExpression <* symbolic ';') mempty str of
  Success x -> Right x
  Failure err -> Left err

parseFileContent :: String -> Either ErrInfo String
parseFileContent = Right
