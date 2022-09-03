-- {-# LANGUAGE MultiParamTypeClasses #-}

module Types
  ( Expression (..),
    Type (..),
  )
where

data Expression
  = CAtom String
  | CLambda String Expression
  | EVar String
  | EApplication Expression Expression
  | Athe Expression Type
  deriving (Eq, Show)

data Type = Atom | Arrow Type Type deriving (Eq, Show)
