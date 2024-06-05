module ParserTest where

import Test.HUnit
import Lexer (lex)
import Prelude hiding (lex)
import Parser (lit, num, var, string, localDef, parse)
import AST (Expr(Let))

-- Parser test
testParser0 = TestCase (assertEqual "lit" [("hello", [])] (lit "hello" (lex "hello" 1)))

testParser1 = TestCase (assertEqual "var" [("hello", [])] (var (lex "hello" 1)))

testParser2 = TestCase (assertEqual "num" [(10, [])] (num (lex "10" 1)))

testParser3 = TestCase (assertEqual "string" [("10", [])] (string (lex "\"10\"" 1)))

testParser4 = TestCase (assertEqual "escape char string" [("hello \"", [])] (string (lex " \"hello \\\"\" " 1)))

letExps = [
  "let a = 1 in a",
  "letrec a = a + 1 in a",
  "let \na = 1\nb=2\n in a + b",
  "let a = 1\nb=2\n in a + b"
  ]

caseExp = [
  "case e of \n <1> a -> 1 + 1\n <2> b -> 2 + 2"
  ]

lambdaExp = [
  "\\a b c . a + b + c"
  ]
-- 写 test case 啊

tests = TestList ([
  TestLabel "test parser" testParser0,
  TestLabel "test parser" testParser1,
  TestLabel "test parser" testParser2,
  TestLabel "test parser" testParser3,
  TestLabel "test parser" testParser4
  ]
  ++ map (TestLabel "test parser" . TestCase . assertEqual "let stmt" [] . parse) letExps)
