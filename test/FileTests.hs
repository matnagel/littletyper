{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FileTests
  ( allFileTests,
  )
where

import Data.Either (fromRight, isLeft, isRight)
import qualified Data.Map.Strict as Map
import Parser.Expression (ErrInfo, parseToExpression)
import Parser.File (parseFileContent)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

allFileTests :: IO TestTree
allFileTests = filenames >>= createTests

filenames :: IO [FilePath]
filenames = listDirectory "test/resources"

createTest :: FilePath -> IO TestTree
createTest fp =
  testCase (show fp) <$> do
    content <- readFile $ "test" </> "resources" </> fp
    let expression = parseFileContent content
    return $ assertBool "could not parse" $ isRight expression

createTests :: [FilePath] -> IO TestTree
createTests fps = testGroup "Testing with pie files" <$> sequence (createTest <$> fps)
