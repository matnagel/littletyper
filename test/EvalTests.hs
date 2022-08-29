{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EvalTests
  ( all_eval_tests,
  )
where

import Data.Either (isLeft, isRight, fromRight)
import Data.Map
import qualified Data.Map.Strict as Map
import Data.String
import Evaluation
import ParseTests
import TypeTests
import Parser.Expression
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecking
import Types

all_eval_tests = test_evaluation

convert_string :: String -> Expression
convert_string input = (fromRight (error $ "Could not parse" ++ show input) (parseToExpression input :: Either ErrInfo Expression))

createEvalTest :: String -> String -> String -> TestTree
createEvalTest desc exp result =
  testCase
    desc
    ( assertBool
        ("isType " ++ show exp ++ " computes to " ++ show result)
        (eval (convert_string exp) == Just (convert_string result))
    )

test_evaluation =
  testGroup
    "evaluate expressions"
    [ cTest "atoms evaluate to themselves" "'tock;" "'tock;",
      cTest "a lambda application" "λ(x){x} 'tock;" "'tock;",
      cTest "an iterated lambda application" "λ(x){x} λ(x){x} 'tock;" "'tock;"
    ]
  where
    cTest desc = createEvalTest desc
