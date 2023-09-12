module TypeChecking
  ( isType,
    inferTypeWithContext,
    VariableTypeContext,
    isTypeWithContext,
  )
where

import qualified Data.Map.Strict as Map
import Types (Expression (..), Type (..))

type VariableTypeContext = Map.Map String Type

inferTypeWithContext :: VariableTypeContext -> Expression -> Maybe Type
inferTypeWithContext context (CAtom str) = Just Atom
inferTypeWithContext context (EVar varname) = Map.lookup varname context
inferTypeWithContext _ (AThe _ typ) = Just typ
inferTypeWithContext _ _ = Nothing

isType :: Expression -> Type -> Bool
isType = isTypeWithContext mempty

isTypeWithContext :: VariableTypeContext -> Expression -> Type -> Bool
isTypeWithContext _ (CAtom _) Atom = True
isTypeWithContext context (EVar varname) typ = case Map.lookup varname context of
  Nothing -> False
  Just ityp -> ityp == typ
isTypeWithContext context (CLambda varname exp) (Arrow ta tb) = isTypeWithContext newcontext exp tb
  where
    newcontext = Map.insert varname ta context
isTypeWithContext context (EApplication fun arg) typ = case inferTypeWithContext context arg of
  Nothing -> False
  Just atyp ->
    isTypeWithContext context arg atyp
      && isTypeWithContext context fun (Arrow atyp typ)
isTypeWithContext context (AThe exp atyp) typ =
  (atyp == typ)
    && isTypeWithContext context exp typ
isTypeWithContext _ _ _ = False
