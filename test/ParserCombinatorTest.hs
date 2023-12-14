module ParserCombinatorTest where

import Test.HUnit
import ParserCombinator (alt, next, next3, oneOrMore, oneOrMoreWithSep, zeroOrMore)
import Parser (lit, num, var)
import Lexer (lex)
import Prelude hiding (lex)
import Prelude (const)

testPc0 = TestCase (assertEqual "alt" [("a", [(1, "b")])] (alt [lit "a", lit "b"] (lex "a b" 1)))

testPc1 = TestCase (assertEqual "next" [("a", [])] (next const (lit "a") (lit "b") (lex "a b" 1)))

testPc2 = TestCase (assertEqual "next3" [("c", [])] (next3 (\_ _ x -> x) (lit "a") (lit "b") (lit "c") (lex "a b c" 1)))

testPc3 = TestCase (assertEqual "zeroOrMore" [(["a", "a", "a"], [(1, "c")])] (zeroOrMore (lit "a") (lex "a a a c" 1)))

testPc4 = TestCase (assertEqual "zeroOrMore" [([], [(1, "c")])] (zeroOrMore (lit "a") (lex " c" 1)))

testPc5 = TestCase (assertEqual "oneOrMore" [(["a", "a", "a"], [(1, "c")])] (oneOrMore (lit "a") (lex "a a a c" 1)))

testPc6 = TestCase (assertEqual "oneOrMoreWithSep" [(["a", "a", "a"], [(1, "c")])] (oneOrMoreWithSep (lit "a") (lit ",") (lex "a , a , a, c" 1)))

tests :: Test
tests = TestList [
      TestLabel "test parser combinator" testPc0,
      TestLabel "test parser combinator" testPc1,
      TestLabel "test parser combinator" testPc2,
      TestLabel "test parser combinator" testPc3,
      TestLabel "test parser combinator" testPc4,
      TestLabel "test parser combinator" testPc5,
      TestLabel "test parser combinator" testPc6
  ]