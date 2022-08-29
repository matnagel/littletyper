module Parser.Token
  ( tokenAtom,
    tokenVariable,
    tokenIdentifier,
  )
where

import Control.Monad
import Text.Trifecta
import Types

protectedKeywords = ["lambda"]

tokenize :: Parser a -> Parser a
tokenize p = p <* whiteSpace

tokenAtom :: Parser Expression
tokenAtom = tokenize $ CAtom <$> (char '\'' >> some letter)

identifier :: Parser String
identifier = do
  string <- (:) <$> letter <*> many alphaNum
  guard $ notElem string protectedKeywords
  return string

tokenIdentifier :: Parser String
tokenIdentifier = tokenize $ identifier

tokenVariable :: Parser Expression
tokenVariable = tokenize $ EVar <$> identifier
