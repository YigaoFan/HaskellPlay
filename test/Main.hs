module Main where

import Control.Exception (assert)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import Lexer (lex)
import Prelude hiding (lex)
import Parser (lit, var, num)
import ParserCombinator (alt, next, next3, oneOrMore, oneOrMoreWithSep, zeroOrMore)
import Prelude (const)

-- oneOrMoreWithSep behavior changed TODO
-- Lexer test
testLexer0 = TestCase (assertEqual "empty" (lex "" 1) [])
testLexer1 = TestCase (assertEqual "digit" (lex "123" 1) [(1, "123")])
testLexer2 = TestCase (assertEqual "digit" (lex " 123 " 1) [(1, "123")])
testLexer3 = TestCase (assertEqual "var" (lex " abc " 1) [(1, "abc")])
testLexer4 = TestCase (assertEqual "comment" (lex "abc || this is comment" 1) [(1, "abc")])
testLexer5 = TestCase (assertEqual "comment" (lex "abc || this is comment\n" 1) [(1, "abc")])
testLexer6 = TestCase (assertEqual "comment" (lex "abc \n|| this is comment\n" 1) [(1, "abc")])
testLexer7 = TestCase (assertEqual "two char operator" (lex "== " 1) [(1, "==")])
testLexer8 = TestCase (assertEqual "var" (lex " abc \n cde" 1) [(1, "abc"), (2, "cde")])

testLexer9 = TestCase (assertEqual "lex special char" (lex " <1>" 1) [(1, "<"), (1, "1"), (1, ">")])

-- Parser test
testParser0 = TestCase (assertEqual "lit" (lit "hello" (lex "hello" 1)) [("hello", [])])
testParser1 = TestCase (assertEqual "var" (var (lex "hello" 1)) [("hello", [])])
testParser2 = TestCase (assertEqual "num" (num (lex "10" 1)) [(10, [])])

-- Parser combinator test
testPc0 = TestCase (assertEqual "alt" (alt [lit "a", lit "b"] (lex "a b" 1)) [("a", [(1, "b")])])
testPc1 = TestCase (assertEqual "next" (next const (lit "a") (lit "b") (lex "a b" 1)) [("a", [])])
testPc2 = TestCase (assertEqual "next3" (next3 (\_ _ x -> x) (lit "a") (lit "b") (lit "c") (lex "a b c" 1)) [("c", [])])
testPc3 = TestCase (assertEqual "zeroOrMore" (zeroOrMore (lit "a") (lex "a a a c" 1)) [(["a", "a", "a"], [(1, "c")])])
testPc4 = TestCase (assertEqual "zeroOrMore" (zeroOrMore (lit "a") (lex " c" 1)) [([], [(1, "c")])])
testPc5 = TestCase (assertEqual "oneOrMore" (oneOrMore (lit "a") (lex "a a a c" 1)) [(["a", "a", "a"], [(1, "c")])])
testPc6 = TestCase (assertEqual "oneOrMoreWithSep" (oneOrMoreWithSep (lit "a") (lit ",") (lex "a , a , a c" 1)) [(["a", "a", "a"], [(1, "c")])])
tests :: Test
tests = TestList [
  TestLabel "test lexer" testLexer0,
  TestLabel "test lexer" testLexer1,
  TestLabel "test lexer" testLexer2,
  TestLabel "test lexer" testLexer3,
  TestLabel "test lexer" testLexer4,
  TestLabel "test lexer" testLexer5,
  TestLabel "test lexer" testLexer6,
  TestLabel "test lexer" testLexer7,
  TestLabel "test lexer" testLexer8,
  TestLabel "test lexer" testLexer9,

  TestLabel "test parser" testParser0,
  TestLabel "test parser" testParser1,
  TestLabel "test parser" testParser2,

  TestLabel "test parser combinator" testPc0,
  TestLabel "test parser combinator" testPc1,
  TestLabel "test parser combinator" testPc2,
  TestLabel "test parser combinator" testPc3,
  TestLabel "test parser combinator" testPc4,
  TestLabel "test parser combinator" testPc5,
  TestLabel "test parser combinator" testPc6
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess