{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ParseTests (
    all_parse_tests
) where
import Test.Tasty
import Test.Tasty.HUnit
import Data.Either (isLeft, isRight, fromRight)
import Data.String

import Data.Map

import Types

import Parser.Expression
import TypeChecking
import Evaluation

import qualified Data.Map.Strict as Map

all_parse_tests = testGroup "Check that parsing works correctly" [test_parse_expressions, test_parse_fail, test_parse_correctly]

createParsableTest :: String -> String -> TestTree
createParsableTest desc input = testCase desc (assertBool
    ("Does not parse the input: " ++ show input)
    (isRight (parseToExpression input :: Either ErrInfo Expression)))

test_parse_expressions = testGroup "parsing sample expressions" [
     cTest "atom" "'bla;"
    ,cTest "ignore whitespaces" "'bla\t\n;"
    ,cTest "ignore useless parens" "('bla);"
    ,cTest "ignore useless parens with spaces" "( 'bla);"
    ,cTest "A Variable" "(thisisavaria2ble);"
    ,cTest "An application" "bla blip;"
    ,cTest "A lambda" "λ (a b) {'bla};"
    ,cTest "Brackets expression" "λ (a) {a} (λ(x) {x} 'tock);"
    ,cTest "Annotation" "'tock:Atom;"
    ,cTest "Annotation brackets" "'tock:(Atom);"
    ,cTest "Annotation brackets with arrow" "'tock:(Atom->Atom);"
    ,cTest "Annotation arrow" "'tock:Atom->Atom;"
    ]
    where cTest = createParsableTest

createParsableFailTest :: String -> String -> TestTree
createParsableFailTest desc input = testCase desc (assertBool
    ("Parse should fail for input: " ++ show input)
    (isLeft (parseToExpression input :: Either ErrInfo Expression)))

test_parse_fail = testGroup "not parsing incorrect expressions" [
    cFailTest "dash in atom" "'bl-a;"
    ,cFailTest "variables should not start with a number" "2abc;"
    ,cFailTest "variables are not allowed to be protected keywords" "lambda;"
    ]
    where cFailTest = createParsableFailTest

createVerifyParseTest :: String -> Expression -> String -> TestTree
createVerifyParseTest desc exp input = testCase desc (assertEqual
    desc exp $ (fromRight (error $ "Could not parse" ++ show input) (parseToExpression input :: Either ErrInfo Expression)))

test_parse_correctly = testGroup "parsing expression correctly" [
    cTest "a variable" (EVar "x") "x;",
    cTest "an atom" (CAtom "x") "'x;",
    cTest "multiple applications" (EApplication (EVar "x") (EApplication (EVar "y") (EVar "z"))) "x y z;"
    ]
    where cTest = createVerifyParseTest