{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParseTests
  ( allParseTests,
  )
where

import Data.Either (fromRight, isLeft, isRight)
import qualified Data.Map.Strict as Map
import Parser.Expression (ErrInfo, parseToExpression)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Types (Expression (CAtom, EApplication, EVar))

allParseTests = testGroup "Check parsing of expressions" [testParseExpressions, testParseFail, testParseCorrectly]

createParsableTest :: String -> String -> TestTree
createParsableTest desc input =
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
      cTest "A Variable" "(thisisavaria2ble);",
      cTest "An application" "bla blip;",
      cTest "A lambda" "λ (a b) {'bla};",
      cTest "Brackets expression" "λ (a) {a} (λ(x) {x} 'tock);",
      cTest "Annotation" "'tock:Atom;",
      cTest "Annotation brackets" "'tock:(Atom);",
      cTest "Annotation brackets with arrow" "'tock:(Atom->Atom);",
      cTest "Annotation arrow" "'tock:Atom->Atom;"
    ]
  where
    cTest = createParsableTest

createParsableFailTest :: String -> String -> TestTree
createParsableFailTest desc input =
  testCase
    desc
    ( assertBool
        ("Parse should fail for input: " ++ show input)
        (isLeft (parseToExpression input :: Either ErrInfo Expression))
    )

testParseFail :: TestTree
testParseFail =
  testGroup
    "not parsing incorrect expressions"
    [ cFailTest "dash in atom" "'bl-a;",
      cFailTest "variables should not start with a number" "2abc;",
      cFailTest "variables are not allowed to be protected keywords" "lambda;"
    ]
  where
    cFailTest = createParsableFailTest

createVerifyParseTest :: String -> Expression -> String -> TestTree
createVerifyParseTest desc exp input =
  testCase
    desc
    ( assertEqual
        desc
        exp
        (fromRight (error $ "Could not parse" ++ show input) (parseToExpression input :: Either ErrInfo Expression))
    )

testParseCorrectly :: TestTree
testParseCorrectly =
  testGroup
    "parsing expression correctly"
    [ cTest "a variable" (EVar "x") "x;",
      cTest "an atom" (CAtom "x") "'x;",
      cTest "multiple applications" (EApplication (EVar "x") (EApplication (EVar "y") (EVar "z"))) "x y z;"
    ]
  where
    cTest = createVerifyParseTest
