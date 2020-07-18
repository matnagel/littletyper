module TypeChecking (
    isType
    )
    where

import qualified Data.Map.Strict as Map

import Control.Applicative
import Control.Monad

import Types

import Text.Trifecta

type Context = Map.Map String Type

isType :: Expression Atom -> Type -> Bool
isType = isTypeWithContext mempty

isTypeWithContext :: Context -> Expression Atom -> Type -> Bool
isTypeWithContext _ (CAtom _) Atom = True
isTypeWithContext context (EVar varname) typ  = case Map.lookup varname context of
    Nothing -> False
    Just ltyp -> (ltyp == typ)
isTypeWithContext context (CLambda varname exp) (Arrow ta tb) = isTypeWithContext newcontext exp tb
    where newcontext = Map.insert varname ta context
isTypeWithContext _ _ _ = False

