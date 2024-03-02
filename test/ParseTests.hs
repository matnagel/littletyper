{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParseTests
  ( allParseTests,
  )
where

import Data.Either (fromRight, isLeft, isRight)
import qualified Data.Map.Strict as Map
import Parser.File (ErrInfo, parseToDefinition, parseToExpression)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Types (Expression (CAtom, EApplication, EVar))

allParseTests :: TestTree
allParseTests = testGroup "Check parsing of expressions" [testParseExpressions, testParseExpressionFail, testParseExpressionCorrectly, testParseDefinition]

createParseExpressionTest :: String -> String -> TestTree
createParseExpressionTest desc input =
  testCase
    desc
    ( assertBool
        ("Does not parse the input: " ++ show input)
        (isRight (parseToExpression input :: Either ErrInfo Expression))
    )

testParseExpressions :: TestTree
testParseExpressions =
  testGroup
    "parsing sample expressions"
    [ cTest "atom" "'bla;",
      cTest "ignore whitespaces" "'bla\t\n;",
      cTest "ignore useless parens" "('bla);",
      cTest "ignore useless parens with spaces" "( 'bla);",
      cTest "a variable" "(thisisavaria2ble);",
      cTest "an application" "bla blip;",
      cTest "a lambda" "λ (a b) {'bla};",
      cTest "brackets expression" "λ (a) {a} (λ(x) {x} 'tock);",
      cTest "annotation" "'tock:Atom;",
      cTest "annotation brackets" "'tock:(Atom);",
      cTest "annotation brackets with arrow" "'tock:(Atom->Atom);",
      cTest "annotation arrow" "'tock:Atom->Atom;"
    ]
  where
    cTest = createParseExpressionTest

createParseExpressionFailTest :: String -> String -> TestTree
createParseExpressionFailTest desc input =
  testCase
    desc
    ( assertBool
        ("Parse should fail for input: " ++ show input)
        (isLeft (parseToExpression input :: Either ErrInfo Expression))
    )

testParseExpressionFail :: TestTree
testParseExpressionFail =
  testGroup
    "not parsing incorrect expressions"
    [ cFailTest "dash in atom" "'bl-a;",
      cFailTest "variables should not start with a number" "2abc;",
      cFailTest "variables are not allowed to be protected keywords" "lambda;"
    ]
  where
    cFailTest = createParseExpressionFailTest

createVerifyParseExpressionTest :: String -> Expression -> String -> TestTree
createVerifyParseExpressionTest desc exp input =
  testCase
    desc
    ( assertEqual
        desc
        exp
        (fromRight (error $ "Could not parse" ++ show input) (parseToExpression input :: Either ErrInfo Expression))
    )

testParseExpressionCorrectly :: TestTree
testParseExpressionCorrectly =
  testGroup
    "parsing expression correctly"
    [ cTest "a variable" (EVar "x") "x;",
      cTest "an atom" (CAtom "x") "'x;",
      cTest "multiple applications" (EApplication (EVar "x") (EApplication (EVar "y") (EVar "z"))) "x y z;"
    ]
  where
    cTest = createVerifyParseExpressionTest

createParseDefinitionTest :: String -> String -> TestTree
createParseDefinitionTest desc input =
  testCase
    desc
    ( assertBool
        ("Does not parse the input: " ++ show input)
        (isRight (parseToDefinition input))
    )

testParseDefinition :: TestTree
testParseDefinition =
  testGroup
    "parsing sample blocks"
    [ cTest "typed variable" "const foo : Atom = 'tock;"
    ]
  where
    cTest = createParseDefinitionTest
