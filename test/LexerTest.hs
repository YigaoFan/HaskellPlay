module LexerTest where

import Test.HUnit
import Lexer (lex)
import Prelude hiding (lex)

testLexer0 = TestCase (assertEqual "empty" [] (lex "" 1))

testLexer1 = TestCase (assertEqual "digit" [(1, "123")] (lex "123" 1))

testLexer2 = TestCase (assertEqual "digit" [(1, "123")] (lex " 123 " 1))

testLexer3 = TestCase (assertEqual "var" [(1, "abc")] (lex " abc " 1))

testLexer4 = TestCase (assertEqual "comment" [(1, "abc")] (lex "abc || this is comment" 1))

testLexer5 = TestCase (assertEqual "comment" [(1, "abc")] (lex "abc || this is comment\n" 1))

testLexer6 = TestCase (assertEqual "comment" [(1, "abc")] (lex "abc \n|| this is comment\n" 1))

testLexer7 = TestCase (assertEqual "two char operator" [(1, "==")] (lex "== " 1))

testLexer8 = TestCase (assertEqual "var" [(1, "abc"), (2, "cde")] (lex " abc \n cde" 1))

testLexer9 = TestCase (assertEqual "lex special char" [(1, "<"), (1, "1"), (1, ">")] (lex " <1>" 1))

testLexer10 = TestCase (assertEqual "lex unline name" [(1, "_abc")] (lex " _abc" 1))

testLexer11 = TestCase (assertEqual "string" [(1, "\"hello world\"")] (lex " \"hello world\" " 1))

testLexer12 = TestCase (assertEqual "escape char string" [(1, "\"hello \\\"\"")] (lex " \"hello \\\"\" " 1))

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
  TestLabel "test lexer" testLexer10,
  TestLabel "test lexer" testLexer11,
  TestLabel "test lexer" testLexer12
  ]