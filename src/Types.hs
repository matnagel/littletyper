-- {-# LANGUAGE MultiParamTypeClasses #-}

module Types (
    Expression (..),
    Atom (..),
    Type (..)
    )
    where

import Data.Map.Strict as Map

newtype Atom = MkAtom String deriving (Eq, Show)

data Expression = CAtom Atom
                | CLambda String Expression
                | EVar String
                | EApplication Expression Expression
                | Athe Expression Type
                 deriving (Eq, Show)

data Type = Atom | Arrow Type Type deriving (Eq, Show)
