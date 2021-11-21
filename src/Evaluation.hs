module Evaluation (
    eval,
    evalWithContext
    )
    where

import qualified Data.Map.Strict as Map

import Control.Applicative
import Control.Monad

import Types

type VariableExpressionContext = Map.Map String Expression

eval :: Expression -> Maybe Expression
eval = evalWithContext mempty

evalWithContext :: VariableExpressionContext -> Expression -> Maybe Expression
evalWithContext context atom@(CAtom _) = return atom
evalWithContext context fun@(CLambda _ _) = return fun
evalWithContext context (EVar varname) = Map.lookup varname context >>= evalWithContext context 
evalWithContext context (EApplication fun arg) = case evalWithContext context fun of
    Nothing -> Nothing
    Just (CLambda varname exp) -> evalWithContext (Map.insert varname arg context) exp
    Just _ -> Nothing
evalWithContext _ _ = Nothing 
