{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Either (isLeft, isRight)
import qualified Data.Map.Strict as Map
import EvalTests (allEvalTests)
import FileTests (allFileTests)
import ParseTests (allParseTests)
import Test.Tasty (TestTree, defaultMain, testGroup)
import TypeTests (allTypeTests)

main :: IO ()
main = allTests >>= defaultMain

allTests :: IO TestTree
allTests = do
  iot <- ioTests
  return $
    testGroup
      "Tests"
      [ allParseTests,
        allTypeTests,
        allEvalTests,
        iot
      ]

ioTests :: IO TestTree
ioTests = allFileTests
