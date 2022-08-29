{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Either (isLeft, isRight)
import Data.Map
import qualified Data.Map.Strict as Map
import Data.String
import EvalTests
import Evaluation
import ParseTests
import Parser.Expression
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecking
import TypeTests
import Types

main = defaultMain all_tests

all_tests :: TestTree
all_tests =
  testGroup
    "Tests"
    [ all_parse_tests,
      all_type_tests,
      all_eval_tests
    ]
