{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Data.Either (isLeft, isRight)
import Data.String

import Types

import ParseExpression
import TypeChecking

instance (Parseable a) => IsString (Expression a) where
    fromString str = case parseToExpression str of
        Right exp -> exp

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
         isTypeTest
        ]
    return ()


createParsableTest :: String -> String -> Test
createParsableTest desc input = TestCase (assertBool
    ("Does not parse: " ++ desc ++ " - Input: " ++ show input)
    (isRight $ (parseToExpression input :: Either ErrInfo (Expression Atom))))

createParsableFailTest :: String -> String -> Test
createParsableFailTest desc input = TestCase (assertBool
    ("Parse should fail: " ++ desc ++ " - Input: " ++ show input)
    (isLeft $ (parseToExpression input :: Either ErrInfo (Expression Atom))))

parseExpressionTest = TestLabel "Parse Expressions" $ TestList [
     cTest "atom" "'bla;"
    ,cFailTest "dash in atom" "'bl-a;"
    ,cFailTest "variables should not start with a number" "2abc;"
    ,cTest "ignore whitespaces" "'bla\t\n;"
    ,cTest "ignore useless parens" "('bla);"
    ,cTest "ignore useless parens with spaces" "( 'bla);"
    ,cTest "A Variable" "(thisisavaria2ble);"
    ,cTest "An application" "bla blip;"
    ,cTest "A lambda" "λ (a b) {'bla};"
        ]
    where cTest = createParsableTest
          cFailTest = createParsableFailTest

createVerifyParseTest :: String -> Expression Atom -> String -> Test
createVerifyParseTest desc exp input = TestCase (assertEqual
    desc exp $ fromString input)

parseCorrectTest = TestLabel "Parses correct results" $ TestList [
    cTest "Variable" (EVar "x") "x;",
    cTest "Atom" (CAtom "x") "'x;",
    cTest "Multiapplication" (EApplication (EVar "x") (EApplication (EVar "y") (EVar "z"))) "x y z;"
    ]
    where cTest = createVerifyParseTest


createIsTypeTest :: String -> Expression Atom -> Type -> Test
createIsTypeTest desc exp typ = TestCase (assertBool
    ("isType" ++ desc ++ " - Input: " ++ show exp ++ " : " ++ show typ)
    (isType exp typ))

createIsTypeFailTest :: String -> Expression Atom -> Type -> Test
createIsTypeFailTest desc exp typ = TestCase (assertBool
    ("isType should fail - " ++ desc ++ ": " ++ show exp ++ " : " ++ show typ)
    (not $ isType exp typ))


isTypeTest = TestLabel "Parses correct results" $ TestList [
    cTest "Atom" (CAtom "x") Atom,
    cFailTest "Variable" (EVar "x") Atom,
    cTest "Lambda" "λ(x){'tock};" (Arrow Atom Atom),
    cFailTest "Lambda" "λ(x y){'tock};" (Arrow Atom Atom),
    cTest "Identity Lambda" "λ(x){x};" (Arrow Atom Atom)
    ]
    where cTest = createIsTypeTest
          cFailTest = createIsTypeFailTest

