{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Either (isLeft, isRight)
import Data.Map
import qualified Data.Map.Strict as Map
import Data.String
import EvalTests
import Evaluation
import FileTests
import ParseTests
import Parser.Expression
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecking
import TypeTests
import Types

main = all_tests >>= defaultMain

all_tests :: IO TestTree
all_tests = do
  iot <- io_tests
  return $
    testGroup
      "Tests"
      [ all_parse_tests,
        all_type_tests,
        all_eval_tests,
        iot
      ]

io_tests :: IO TestTree
io_tests = all_file_tests
