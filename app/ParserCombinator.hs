module ParserCombinator where

import Data.Bool (Bool)
import Data.String (String)
import GHC.Int (Int)
import Lexer (Token)
import Data.List ((++))
import GHC.Base (const)

type Parser t = [Token] -> [(t, [Token])]

alt :: Parser t -> Parser t -> Parser t
alt p0 p1 toks = p0 toks ++ p1 toks

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

oneOrMore :: Parser a -> Parser [a]
oneOrMore p input =
  [ (v : vs, remain) | (v, toks) <- p input, (vs, remain) <- oneOrMore p toks -- I am talent! list comprehension from where stmt
  -- up is right?
  -- 这种第二个式子以来第一个式子的计算结果的，第一个式子结果是空列表，第二还会算吗？
  ]

empty :: [a] -> Parser [a]
empty as toks = [(as, toks)]

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = alt (oneOrMore p) (empty []) -- how this [] work?

apply :: Parser a -> (a -> b) -> Parser b
apply p convert input = [(convert v, toks) | (v, toks) <- p input]

oneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
oneOrMoreWithSep p0 p1 = oneOrMore (next const p0 p1)

satisfy :: (String -> Bool) -> Parser String
satisfy pred ((_, str) : ts) = [(str, ts) | pred str]