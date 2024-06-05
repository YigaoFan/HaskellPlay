module ParserCombinator where

import Data.Bool (Bool)
import Data.String (String)
import GHC.Int (Int)
import Lexer (Token)
import Data.List ((++))
import GHC.Base (const)
import Data.Bifunctor (Bifunctor (first))

type Parser t = [Token] -> [(t, [Token])]

alt :: [Parser t] -> Parser t
alt [] _ = []
alt (p : ps) toks = p toks ++ alt ps toks

option :: Parser a -> Parser (Maybe a)
option p toks =
  let r = p toks in
    if null r
      then [(Nothing, toks)]
      else map (Data.Bifunctor.first Just) r

next :: (t0 -> t1 -> t2) -> Parser t0 -> Parser t1 -> Parser t2
next combine p0 p1 tokens =
  [ (combine v0 v1, toks1) | (v0, toks0) <- p0 tokens, (v1, toks1) <- p1 toks0 -- result of p0 tokens is list, why can unpack like this? because in [] is iteration
  ]

next3 :: (t0 -> t1 -> t2 -> t3) -> Parser t0 -> Parser t1 -> Parser t2 -> Parser t3
next3 combine p0 p1 p2 tokens =
  [ (combine v0 v1 v2, toks2) | (v0, toks0) <- p0 tokens, (v1, toks1) <- p1 toks0, (v2, toks2) <- p2 toks1
  ]

asTuple :: a -> b -> c -> (a, b, c)
asTuple a0 a1 a2 = (a0, a1, a2)

next4 :: (t0 -> t1 -> t2 -> t3 -> t4) -> Parser t0 -> Parser t1 -> Parser t2 -> Parser t3 -> Parser t4
next4 combine p0 p1 p2 p3 tokens =
  [ (combine v0 v1 v2 v3, toks1) | ((v0, v1, v2), toks0) <- next3 asTuple p0 p1 p2 tokens, (v3, toks1) <- p3 toks0
  ]

asTuple4 :: a -> b -> c -> d -> (a, b, c, d)
asTuple4 a b c d = (a, b, c, d)

next5 :: (t0 -> t1 -> t2 -> t3 -> t4 -> t5) -> Parser t0 -> Parser t1 -> Parser t2 -> Parser t3 -> Parser t4 -> Parser t5
next5 combine p0 p1 p2 p3 p4 tokens =
  [ (combine v0 v1 v2 v3 v4, toks1) | ((v0, v1, v2, v3), toks0) <- next4 asTuple4 p0 p1 p2 p3 tokens, (v4, toks1) <- p4 toks0
  ]

oneOrMore :: Parser a -> Parser [a]
oneOrMore p input =
  [ (v : vs, remain) | (v, toks) <- p input, (vs, remain) <- zeroOrMore p toks -- I am talent! list comprehension from where stmt
  -- up is right?
  -- 这种第二个式子以来第一个式子的计算结果的，第一个式子结果是空列表，第二还会算吗？
  ]

empty :: a -> Parser a
empty a toks = [(a, toks)]

-- keep one parser result
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p input =
  if null r
    then empty [] input
    else r
  where r = oneOrMore p input

apply :: Parser a -> (a -> b) -> Parser b
apply p convert input = [(convert v, toks) | (v, toks) <- p input]

-- | oneOrMoreWithSep a sep parse for "a sep a"
oneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
oneOrMoreWithSep p0 p1 = next (:) p0 (zeroOrMore pair)
  where pair = next (\_ y -> y) p1 p0

satisfy :: (String -> Bool) -> Parser String
satisfy pred [] = []
satisfy pred ((_, s) : ts) = [(s, ts) | pred s]