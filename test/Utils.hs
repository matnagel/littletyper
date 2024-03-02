module Utils
  ( convertString,
  )
where

import Data.Either (fromRight)
import Parser.File (ErrInfo, parseToExpression)
import Types (Expression)

convertString :: String -> Expression
convertString input = fromRight (error $ "Could not parse" ++ show input) (parseToExpression input :: Either ErrInfo Expression)
