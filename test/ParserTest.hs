{-# LANGUAGE TemplateHaskell, CPP #-}
module ParserTest where

import Test.HUnit
import Lexer (lex)
import Prelude hiding (lex)
import Parser (lit, num, var, string, localDef, parse, takeFirstFullParse, lambda)
import AST (Expr(Let, Lambda))
import TypeUtil (shouldMatch)
import Data.Function ((&))
import ParserCombinator (Parser)
-- Parser test

testParser0 = TestCase (assertEqual "lit" [("hello", [])] (lit "hello" (lex "hello" 1)))

testParser1 = TestCase (assertEqual "var" [("hello", [])] (var (lex "hello" 1)))

testParser2 = TestCase (assertEqual "num" [(10, [])] (num (lex "10" 1)))

testParser3 = TestCase (assertEqual "string" [("10", [])] (string (lex "\"10\"" 1)))

testParser4 = TestCase (assertEqual "escape char string" [("hello \"", [])] (string (lex " \"hello \\\"\" " 1)))

letExps :: [String]
letExps = [
  "let a = a + 1 in a",
  "let \na = 1\nb=2\n in a + b",
  "let a = 1\nb=2\n in a + b"
  ]

caseExp = [
  "case e of \n <1> a -> 1 + 1\n <2> b -> 2 + 2"
  ]

lambdaExp = [
  "\\a b c . a + b + c",
  "\\a b c . let a = 1 in a + b + c"
  ]
-- 写 test case 啊

#define match(pattern) & $(shouldMatch [p| pattern |])
#define expect(pattern) $(shouldMatch [p| pattern |])
testLetExp = TestCase (localDef (lex (head letExps) 1) & $(shouldMatch [p| (Let {}, []) : xs |]))

testCases = [
  testParser1,
  testParser2,
  testParser3,
  testParser4
  ]
  ++ map (TestCase . expect(Let False _ _) . takeFirstFullParse . localDef . (`lex` 1)) letExps
  ++ map (TestCase . expect((Lambda ["a", "b", "c"] _)) . takeFirstFullParse . lambda . (`lex` 1)) lambdaExp

tests = TestList testCases