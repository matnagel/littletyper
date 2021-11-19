{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Either (isLeft, isRight)
import Data.String

import Data.Map

import Types

import ParseExpression
import TypeChecking
import Evaluation

import qualified Data.Map.Strict as Map

instance IsString Expression where
    fromString str = case parseToExpression str of
        Right exp -> exp
        Left str -> error $ "Static expression does not parse: " ++ show str

instance (IsString Atom) where
    fromString = MkAtom


createParsableTest :: String -> String -> TestTree
createParsableTest desc input = testCase desc (assertBool
    ("Does not parse the input: " ++ show input)
    (isRight $ (parseToExpression input :: Either ErrInfo Expression)))

createParsableFailTest :: String -> String -> TestTree
createParsableFailTest desc input = testCase desc (assertBool
    ("Parse should fail for input: " ++ show input)
    (isLeft $ (parseToExpression input :: Either ErrInfo Expression)))

test_parseExpressions = testGroup "parsing sample expressions" [
     cTest "atom" "'bla;"
    ,cFailTest "dash in atom" "'bl-a;"
    ,cFailTest "variables should not start with a number" "2abc;"
    ,cFailTest "variables are not allowed to be protected keywords" "lambda;"
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
          cFailTest = createParsableFailTest

createVerifyParseTest :: String -> Expression -> String -> TestTree
createVerifyParseTest desc exp input = testCase desc (assertEqual
    desc exp $ fromString input)

test_parseCorrectly = testGroup "parsing expression correctly" [
    cTest "a variable" (EVar "x") "x;",
    cTest "an atom" (CAtom "x") "'x;",
    cTest "multiple applications" (EApplication (EVar "x") (EApplication (EVar "y") (EVar "z"))) "x y z;"
    ]
    where cTest = createVerifyParseTest


context :: Map String Type
context = fromList [("x", Atom)]

createInferTypeTest :: String -> Expression -> Maybe Type -> TestTree
createInferTypeTest desc exp typ = testCase desc (assertEqual
    ("isType " ++ show exp ++ " : " ++ show typ)
    typ
    (inferTypeWithContext context exp))

test_inferType = testGroup "infering the type of an expression" [
    cTest "an atom" "'tock;" (Just Atom),
    cTest "a lambda" "λ(x){'tock};" Nothing,
    cTest "variable in context" "x;" (Just Atom),
    cTest "unknown variable" "y;" Nothing,
    cTest "a typed lambda" "λ(x){x} : Atom -> Atom;" $ Just (Arrow Atom Atom)
    ]
    where cTest = createInferTypeTest


createIsTypeTest :: String -> Expression -> Type -> TestTree
createIsTypeTest desc exp typ = testCase desc (assertBool
    ("isType " ++ show exp ++ " : " ++ show typ)
    (isType exp typ))

createIsTypeFailTest :: String -> Expression -> Type -> TestTree
createIsTypeFailTest desc exp typ = testCase desc (assertBool
    ("isType should fail " ++ show exp ++ " : " ++ show typ)
    (not $ isType exp typ))


test_isType = testGroup "checking that an expression has a type" [
    cTest "an atom" (CAtom "x") Atom,
    cTest "an atom" "'atom:Atom;" Atom,
    cFailTest "a function is not an atom" "'atom:Atom->Atom;" Atom,
    cFailTest "a variable" (EVar "x") Atom,
    cTest "a lambda" "λ(x){'tock};" (Arrow Atom Atom),
    cFailTest "another multi-argument lambda" "λ(x y){'tock};" (Arrow Atom Atom),
    cTest "the identity" "λ(x){x};" (Arrow Atom Atom),
    cTest "an application" "λ(x){x} 'tock;" Atom
    ]
    where cTest = createIsTypeTest
          cFailTest = createIsTypeFailTest

createIsTypeWithContextTest :: String -> VariableTypeContext -> Expression -> Type -> TestTree
createIsTypeWithContextTest desc cont exp typ = testCase desc (assertBool
    ("isType " ++ show exp ++ " : " ++ show typ)
    (isTypeWithContext cont exp typ))

testContext = fromList [
    ("a", Atom),
    ("b", Atom)]

test_isTypeWithContext = testGroup "an expression has a type given a context" [
    cTest "an atom" "a;" Atom,
    cTest "an application" "λ(x){a} b;" Atom,
    cTest "an interated application" "λ(x){a} (λ(x){a} b : Atom);" Atom
    ]
    where cTest desc = createIsTypeWithContextTest desc testContext

createEvalTest :: String -> Expression -> Expression -> TestTree
createEvalTest desc exp result = testCase desc (assertBool
    ("isType " ++ show exp ++ " computes to " ++ show result)
    (eval exp == Just result))

test_evaluation = testGroup "evaluate expressions" [
    cTest "atoms evaluate to themselves" "'tock;" "'tock;",
    cTest "a lambda application" "λ(x){x} 'tock;" "'tock;",
    cTest "an iterated lambda application" "λ(x){x} λ(x){x} 'tock;" "'tock;"
    ]
    where cTest desc = createEvalTest desc
