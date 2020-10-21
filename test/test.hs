{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
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

runList :: [Test] -> IO ()
runList [] = return ()
runList (t:ts) = do
    stats <- runTestTT t
    if errors stats + failures stats > 0
        then return ()
        else runList ts

main::IO()
main = do
    runList [
         parseExpressionTest,
         parseCorrectTest,
         inferTypeTest,
         isTypeTest,
         isTypeWithContextTest,
         evalTest
        ]
    return ()


createParsableTest :: String -> String -> Test
createParsableTest desc input = TestCase (assertBool
    ("Does not parse: " ++ desc ++ " - Input: " ++ show input)
    (isRight $ (parseToExpression input :: Either ErrInfo Expression)))

createParsableFailTest :: String -> String -> Test
createParsableFailTest desc input = TestCase (assertBool
    ("Parse should fail: " ++ desc ++ " - Input: " ++ show input)
    (isLeft $ (parseToExpression input :: Either ErrInfo Expression)))

parseExpressionTest = TestLabel "Parse Expressions" $ TestList [
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

createVerifyParseTest :: String -> Expression -> String -> Test
createVerifyParseTest desc exp input = TestCase (assertEqual
    desc exp $ fromString input)

parseCorrectTest = TestLabel "Parses correct results" $ TestList [
    cTest "Variable" (EVar "x") "x;",
    cTest "Atom" (CAtom "x") "'x;",
    cTest "Multiapplication" (EApplication (EVar "x") (EApplication (EVar "y") (EVar "z"))) "x y z;"
    ]
    where cTest = createVerifyParseTest


context :: Map String Type
context = fromList [("x", Atom)]

createInferTypeTest :: String -> Expression -> Maybe Type -> Test
createInferTypeTest desc exp typ = TestCase (assertEqual
    ("isType" ++ desc ++ " - Input: " ++ show exp ++ " : " ++ show typ)
    typ
    (inferTypeWithContext context exp))

inferTypeTest = TestLabel "inferType" $ TestList [
    cTest "Atom" "'tock;" (Just Atom),
    cTest "Lambda" "λ(x){'tock};" Nothing,
    cTest "Variable" "x;" (Just Atom),
    cTest "Variable" "y;" Nothing,
    cTest "Lambda" "λ(x){x} : Atom -> Atom;" $ Just (Arrow Atom Atom)
    ]
    where cTest = createInferTypeTest


createIsTypeTest :: String -> Expression -> Type -> Test
createIsTypeTest desc exp typ = TestCase (assertBool
    ("isType" ++ desc ++ " - Input: " ++ show exp ++ " : " ++ show typ)
    (isType exp typ))

createIsTypeFailTest :: String -> Expression -> Type -> Test
createIsTypeFailTest desc exp typ = TestCase (assertBool
    ("isType should fail - " ++ desc ++ ": " ++ show exp ++ " : " ++ show typ)
    (not $ isType exp typ))


isTypeTest = TestLabel "isType" $ TestList [
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

createIsTypeWithContextTest :: String -> VariableTypeContext -> Expression -> Type -> Test
createIsTypeWithContextTest desc cont exp typ = TestCase (assertBool
    ("isType" ++ desc ++ " - Input: " ++ show exp ++ " : " ++ show typ)
    (isTypeWithContext cont exp typ))

testContext = fromList [
    ("a", Atom),
    ("b", Atom)]

isTypeWithContextTest = TestLabel "isType with context" $ TestList [
    cTest "Atom" "a;" Atom,
    cTest "Lambda application" "λ(x){a} b;" Atom,
    cTest "Interated lambda applications" "λ(x){a} (λ(x){a} b : Atom);" Atom
    ]
    where cTest desc = createIsTypeWithContextTest desc testContext

createEvalTest :: String -> Expression -> Expression -> Test
createEvalTest desc exp result = TestCase (assertBool
    ("isType" ++ desc ++ " - Input: " ++ show exp ++ " computes to " ++ show result)
    (eval exp == Just result))

evalTest = TestLabel "evaluation Test" $ TestList [
    cTest "Atom are final" "'tock;" "'tock;",
    cTest "Evaluate lambda" "λ(x){x} 'tock;" "'tock;"
    ]
    where cTest desc = createEvalTest desc
