module Types (
    Expression (..)
    )
    where

import Data.Map.Strict as Map

data Expression = CAtom String
                | CLambda String Expression
                | EVar String
                | EApplication Expression Expression
--              | Athe Expression (Type a)
                 deriving (Eq, Show)
