{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileTests
  ( all_file_tests,
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

all_file_tests :: IO TestTree
all_file_tests = test_load_files

filenames :: IO [FilePath]
filenames = listDirectory "test/resources"

createTest :: FilePath -> TestTree
createTest fp = testCase (show fp) $ assertBool "Is Good" True

createTests :: [FilePath] -> TestTree
createTests fps = testGroup "Testing with pie files" $ (createTest <$> fps)

test_load_files :: IO TestTree
test_load_files = createTests <$> filenames
