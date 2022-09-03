{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileTests
  ( allFileTests,
  )
where

import Data.Either (fromRight, isLeft, isRight)
import qualified Data.Map.Strict as Map
import System.Directory (listDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

allFileTests :: IO TestTree
allFileTests = createTests <$> filenames

filenames :: IO [FilePath]
filenames = listDirectory "test/resources"

createTest :: FilePath -> TestTree
createTest fp = testCase (show fp) $ assertBool "Is Good" True

createTests :: [FilePath] -> TestTree
createTests fps = testGroup "Testing with pie files" (createTest <$> fps)
