{-# LANGUAGE MultiParamTypeClasses #-}

module Types (
    Expression (..),
    Atom (..),
    Type (..)
    )
    where

import Data.Map.Strict as Map

newtype Atom = MkAtom String deriving (Eq, Show)

data Expression a = CAtom a
                | CLambda String (Expression a)
                | EVar String
                | EApplication (Expression a) (Expression a)
--              | Athe Expression (Type a)
                 deriving (Eq, Show)

data Type = Atom | Arrow Type Type | TypeOfVariable String deriving Show
