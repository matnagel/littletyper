{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EvalTests
  ( allEvalTests,
  )
where

import Data.Either (fromRight, isLeft, isRight)
import qualified Data.Map.Strict as Map
import Evaluation (eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import Types (Expression)
import Utils (convertString)

allEvalTests :: TestTree
allEvalTests = testEvaluation

createEvalTest :: String -> String -> String -> TestTree
createEvalTest desc exp result =
  testCase
    desc
    ( assertBool
        ("isType " ++ show exp ++ " computes to " ++ show result)
        (eval (convertString exp) == Just (convertString result))
    )

testEvaluation :: TestTree
testEvaluation =
  testGroup
    "evaluate expressions"
    [ cTest "atoms evaluate to themselves" "'tock;" "'tock;",
      cTest "a lambda application" "λ(x){x} 'tock;" "'tock;",
      cTest "an iterated lambda application" "λ(x){x} λ(x){x} 'tock;" "'tock;"
    ]
  where
    cTest desc = createEvalTest desc
