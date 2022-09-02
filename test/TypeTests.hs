{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TypeTests
  ( allTypeTests,
  )
where

import Data.Either (fromRight, isLeft, isRight)
import Data.Map (Map, fromList)
import qualified Data.Map.Strict as Map
import Parser.Expression (ErrInfo, parseToExpression)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import TypeChecking
  ( VariableTypeContext,
    inferTypeWithContext,
    isType,
    isTypeWithContext,
  )
import Types (Expression, Type (..))

allTypeTests :: TestTree
allTypeTests = testGroup "Check typing of expressions" [testInferType, testIsType, testIsTypeWithContext]

convertString :: String -> Expression
convertString input = fromRight (error $ "Could not parse" ++ show input) (parseToExpression input :: Either ErrInfo Expression)

context :: Map String Type
context = fromList [("x", Atom)]

createInferTypeTest :: String -> String -> Maybe Type -> TestTree
createInferTypeTest desc exp typ =
  testCase
    desc
    ( assertEqual
        ("isType " ++ show exp ++ " : " ++ show typ)
        typ
        (inferTypeWithContext context $ convertString exp)
    )

testInferType :: TestTree
testInferType =
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
        (isType (convertString exp) typ)
    )

createIsTypeFailTest :: String -> String -> Type -> TestTree
createIsTypeFailTest desc exp typ =
  testCase
    desc
    ( assertBool
        ("isType should fail " ++ show exp ++ " : " ++ show typ)
        (not $ isType (convertString exp) typ)
    )

testIsType :: TestTree
testIsType =
  testGroup
    "checking that an expression has a type"
    [ cTest "an atom" "'atom:Atom;" Atom,
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
        (isTypeWithContext cont (convertString exp) typ)
    )

testContext :: Map String Type
testContext =
  fromList
    [ ("a", Atom),
      ("b", Atom)
    ]

testIsTypeWithContext :: TestTree
testIsTypeWithContext =
  testGroup
    "an expression has a type given a context"
    [ cTest "an atom" "a;" Atom,
      cTest "an application" "λ(x){a} b;" Atom,
      cTest "an interated application" "λ(x){a} (λ(x){a} b : Atom);" Atom
    ]
  where
    cTest desc = createIsTypeWithContextTest desc testContext
