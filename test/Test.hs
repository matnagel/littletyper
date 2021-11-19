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

test_parseExpressionTest = testGroup "parsing sample expressions" [
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
createVerifyParseTest desc exp input = testCase "" (assertEqual
    desc exp $ fromString input)

parseCorrectTest = testGroup "Parses correct results" [
    cTest "Variable" (EVar "x") "x;",
    cTest "Atom" (CAtom "x") "'x;",
    cTest "Multiapplication" (EApplication (EVar "x") (EApplication (EVar "y") (EVar "z"))) "x y z;"
    ]
    where cTest = createVerifyParseTest


context :: Map String Type
context = fromList [("x", Atom)]

createInferTypeTest :: String -> Expression -> Maybe Type -> TestTree
createInferTypeTest desc exp typ = testCase "" (assertEqual
    ("isType" ++ desc ++ " - Input: " ++ show exp ++ " : " ++ show typ)
    typ
    (inferTypeWithContext context exp))

inferTypeTest = testGroup "inferType" [
    cTest "Atom" "'tock;" (Just Atom),
    cTest "Lambda" "λ(x){'tock};" Nothing,
    cTest "Variable" "x;" (Just Atom),
    cTest "Variable" "y;" Nothing,
    cTest "Lambda" "λ(x){x} : Atom -> Atom;" $ Just (Arrow Atom Atom)
    ]
    where cTest = createInferTypeTest


createIsTypeTest :: String -> Expression -> Type -> TestTree
createIsTypeTest desc exp typ = testCase "" (assertBool
    ("isType" ++ desc ++ " - Input: " ++ show exp ++ " : " ++ show typ)
    (isType exp typ))

createIsTypeFailTest :: String -> Expression -> Type -> TestTree
createIsTypeFailTest desc exp typ = testCase "" (assertBool
    ("isType should fail - " ++ desc ++ ": " ++ show exp ++ " : " ++ show typ)
    (not $ isType exp typ))


isTypeTest = testGroup "isType" [
    cTest "Atom" (CAtom "x") Atom,
    cTest "Atom" "'atom:Atom;" Atom,
    cFailTest "Atom" "'atom:Atom->Atom;" Atom,
    cFailTest "Variable" (EVar "x") Atom,
    cTest "Lambda" "λ(x){'tock};" (Arrow Atom Atom),
    cFailTest "Lambda" "λ(x y){'tock};" (Arrow Atom Atom),
    cTest "Identity Lambda" "λ(x){x};" (Arrow Atom Atom),
    cTest "Lambda application" "λ(x){x} 'tock;" Atom
    ]
    where cTest = createIsTypeTest
          cFailTest = createIsTypeFailTest

createIsTypeWithContextTest :: String -> VariableTypeContext -> Expression -> Type -> TestTree
createIsTypeWithContextTest desc cont exp typ = testCase "" (assertBool
    ("isType" ++ desc ++ " - Input: " ++ show exp ++ " : " ++ show typ)
    (isTypeWithContext cont exp typ))

testContext = fromList [
    ("a", Atom),
    ("b", Atom)]

isTypeWithContextTest = testGroup "isType with context" [
    cTest "Atom" "a;" Atom,
    cTest "Lambda application" "λ(x){a} b;" Atom,
    cTest "Interated lambda applications" "λ(x){a} (λ(x){a} b : Atom);" Atom
    ]
    where cTest desc = createIsTypeWithContextTest desc testContext

createEvalTest :: String -> Expression -> Expression -> TestTree
createEvalTest desc exp result = testCase "" (assertBool
    ("isType" ++ desc ++ " - Input: " ++ show exp ++ " computes to " ++ show result)
    (eval exp == Just result))

evalTest = testGroup "evaluation Test" [
    cTest "Atom are final" "'tock;" "'tock;",
    cTest "Evaluate lambda" "λ(x){x} 'tock;" "'tock;",
    cTest "Evaluate lambda" "λ(x){x} λ(x){x} 'tock;" "'tock;"
    ]
    where cTest desc = createEvalTest desc
