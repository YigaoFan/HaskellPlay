module Main where

import Control.Exception (assert)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import qualified LexerTest (tests)
import qualified ParserTest (tests)
import qualified ParserCombinatorTest (tests)
import qualified EvaluatorTest (tests)

tests :: Test
tests = TestList [
  TestLabel "Lexer test" LexerTest.tests,
  TestLabel "Parser test" ParserTest.tests,
  TestLabel "Parser Combinator test" ParserCombinatorTest.tests,
  TestLabel "Evaluator test" EvaluatorTest.tests
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess