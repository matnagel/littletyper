{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Either (isLeft, isRight)
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

instance IsString Expression where
  fromString str = case parseToExpression str of
    Right exp -> exp
    Left str -> error $ "Static expression does not parse: " ++ show str

main = defaultMain all_tests

all_tests :: TestTree
all_tests =
  testGroup
    "Tests"
    [ all_parse_tests,
      all_type_tests,
      test_evaluation
    ]

createEvalTest :: String -> Expression -> Expression -> TestTree
createEvalTest desc exp result =
  testCase
    desc
    ( assertBool
        ("isType " ++ show exp ++ " computes to " ++ show result)
        (eval exp == Just result)
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
