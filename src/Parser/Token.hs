module Parser.Token (
    tokenAtom,
    tokenVariable,
    tokenIdentifier
) where

import Text.Trifecta
import Types
import Control.Monad

protectedKeywords = ["lambda"]

tokenize :: Parser a -> Parser a
tokenize p = p <* whiteSpace

tokenAtom :: Parser Expression
tokenAtom = tokenize $ CAtom . MkAtom <$> (char '\'' >> some letter)

identifier :: Parser String
identifier = do
  string <- (:) <$> letter <*> many alphaNum
  guard $ notElem string protectedKeywords
  return string

tokenIdentifier:: Parser String
tokenIdentifier = tokenize $ identifier

tokenVariable :: Parser Expression
tokenVariable = tokenize $ EVar <$> identifier