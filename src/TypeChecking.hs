module TypeChecking (
    isType,
    inferTypeWithContext
    )
    where

import qualified Data.Map.Strict as Map

import Control.Applicative
import Control.Monad

import Types

import Text.Trifecta

type Context = Map.Map String Type

inferTypeWithContext :: Context -> Expression -> Maybe Type
inferTypeWithContext context (CAtom str) = Just Atom
inferTypeWithContext context (EVar varname) = Map.lookup varname context
inferTypeWithContext _ (Athe _ typ) = Just typ
inferTypeWithContext _ _ = Nothing

isType :: Expression -> Type -> Bool
isType = isTypeWithContext mempty

isTypeWithContext :: Context -> Expression -> Type -> Bool
isTypeWithContext _ (CAtom _) Atom = True
isTypeWithContext context var@(EVar _) typ  = case inferTypeWithContext context var of
    Nothing -> False
    Just ityp -> (ityp == typ)
isTypeWithContext context (CLambda varname exp) (Arrow ta tb) = isTypeWithContext newcontext exp tb
    where newcontext = Map.insert varname ta context
isTypeWithContext context (EApplication fun arg) typ = case inferTypeWithContext context arg of
    Nothing -> False
    Just atyp -> (isTypeWithContext context arg atyp) && (isTypeWithContext context fun (Arrow atyp typ))
isTypeWithContext context (Athe exp atyp) typ = (atyp == typ) && isTypeWithContext context exp typ
isTypeWithContext _ _ _ = False

