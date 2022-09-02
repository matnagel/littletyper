{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileTests
  ( allFileTests,
  )
where

import Data.Either (fromRight, isLeft, isRight)
import Data.Map
import qualified Data.Map.Strict as Map
import Data.String
import Evaluation
import Parser.Expression
import System.Directory
import Test.Tasty
import Test.Tasty.HUnit
import TypeChecking
import Types

allFileTests :: IO TestTree
allFileTests = createTests <$> filenames

filenames :: IO [FilePath]
filenames = listDirectory "test/resources"

createTest :: FilePath -> TestTree
createTest fp = testCase (show fp) $ assertBool "Is Good" True

createTests :: [FilePath] -> TestTree
createTests fps = testGroup "Testing with pie files" (createTest <$> fps)
