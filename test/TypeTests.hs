{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeTests (
  all_type_tests
) where

import Data.Either (isLeft, isRight, fromRight)
import Data.Map
import qualified Data.Map.Strict as Map
import Data.String
import Evaluation
import ParseTests
import Parser.Expression
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecking
import Types

all_type_tests = testGroup "Check typing of expressions" [test_infer_type, test_is_type, test_is_type_with_context]

convert_string :: String -> Expression
convert_string input = (fromRight (error $ "Could not parse" ++ show input) (parseToExpression input :: Either ErrInfo Expression))

context :: Map String Type
context = fromList [("x", Atom)]

createInferTypeTest :: String -> String -> Maybe Type -> TestTree
createInferTypeTest desc exp typ =
  testCase
    desc
    ( assertEqual
        ("isType " ++ show exp ++ " : " ++ show typ)
        typ
        (inferTypeWithContext context $ convert_string exp)
    )

test_infer_type =
  testGroup
    "infering the type of an expression"
    [ cTest "an atom" "'tock;" (Just Atom),
      cTest "a lambda" "λ(x){'tock};" Nothing,
      cTest "variable in context" "x;" (Just Atom),
      cTest "unknown variable" "y;" Nothing,
      cTest "a typed lambda" "λ(x){x} : Atom -> Atom;" $ Just (Arrow Atom Atom)
    ]
  where
    cTest = createInferTypeTest

createIsTypeTest :: String -> String -> Type -> TestTree
createIsTypeTest desc exp typ =
  testCase
    desc
    ( assertBool
        ("isType " ++ show exp ++ " : " ++ show typ)
        (isType (convert_string exp) typ)
    )

createIsTypeFailTest :: String -> String -> Type -> TestTree
createIsTypeFailTest desc exp typ =
  testCase
    desc
    ( assertBool
        ("isType should fail " ++ show exp ++ " : " ++ show typ)
        (not $ isType (convert_string exp) typ)
    )

test_is_type =
  testGroup
    "checking that an expression has a type"
    [ 
      cTest "an atom" "'atom:Atom;" Atom,
      cFailTest "a function is not an atom" "'atom:Atom->Atom;" Atom,
      cFailTest "a variable" "x;" Atom,
      cTest "a lambda" "λ(x){'tock};" (Arrow Atom Atom),
      cFailTest "another multi-argument lambda" "λ(x y){'tock};" (Arrow Atom Atom),
      cTest "the identity" "λ(x){x};" (Arrow Atom Atom),
      cTest "an application" "λ(x){x} 'tock;" Atom
    ]
  where
    cTest = createIsTypeTest
    cFailTest = createIsTypeFailTest

createIsTypeWithContextTest :: String -> VariableTypeContext -> String -> Type -> TestTree
createIsTypeWithContextTest desc cont exp typ =
  testCase
    desc
    ( assertBool
        ("isType " ++ show exp ++ " : " ++ show typ)
        (isTypeWithContext cont (convert_string exp) typ)
    )

testContext =
  fromList
    [ ("a", Atom),
      ("b", Atom)
    ]

test_is_type_with_context =
  testGroup
    "an expression has a type given a context"
    [ cTest "an atom" "a;" Atom,
      cTest "an application" "λ(x){a} b;" Atom,
      cTest "an interated application" "λ(x){a} (λ(x){a} b : Atom);" Atom
    ]
  where
    cTest desc = createIsTypeWithContextTest desc testContext