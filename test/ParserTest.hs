module ParserTest where

import Test.HUnit
import Lexer (lex)
import Prelude hiding (lex)
import Parser (lit, num, var, string)

-- Parser test
testParser0 = TestCase (assertEqual "lit" [("hello", [])] (lit "hello" (lex "hello" 1)))

testParser1 = TestCase (assertEqual "var" [("hello", [])] (var (lex "hello" 1)))

testParser2 = TestCase (assertEqual "num" [(10, [])] (num (lex "10" 1)))

testParser3 = TestCase (assertEqual "string" [("10", [])] (string (lex "\"10\"" 1)))

testParser4 = TestCase (assertEqual "escape char string" [("hello \"", [])] (string (lex " \"hello \\\"\" " 1)))

tests = TestList [
  TestLabel "test parser" testParser0,
  TestLabel "test parser" testParser1,
  TestLabel "test parser" testParser2,
  TestLabel "test parser" testParser3,
  TestLabel "test parser" testParser4
  ]
