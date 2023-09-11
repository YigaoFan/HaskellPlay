module Lexer (Token, lex) where

import Data.Char (Char, isAlpha, isDigit, isSpace)
import Data.Foldable (elem)
import Data.List (dropWhile, takeWhile)
import Prelude hiding (lex)
import Data.Bool (Bool, (||))
import Data.String (String)
import GHC.Int (Int)
import Data.Eq ((==), (/=))
import GHC.Num ((+))
import GHC.Base ((&&))

type Token = (Int, String) -- String in token is never empty

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

lex :: String -> Int -> [Token]
lex cs lineNum
  | cs == "" = []
lex (c : cs) lineNum
  | isSpace c =
      case c of
        '\n' -> lex cs (lineNum + 1)
        _ -> lex cs lineNum
  | isDigit c = (lineNum, num) : lex restChars lineNum
  where
    num = c : takeWhile isDigit cs
    restChars = dropWhile isDigit cs
lex (c : cs) lineNum
  | isAlpha c = (lineNum, var) : lex restChars lineNum
  where
    var = c : takeWhile isIdChar cs
    restChars = dropWhile isIdChar cs
lex (c0 : c1 : cs) lineNum -- last line in file which has comment will pass this item?
  | c0 == '|' && c1 == '|' = lex nextLine (lineNum + 1)
  where
    n : nextLine = dropWhile ('\n' /=) cs
lex (c0 : c1 : cs) lineNum
  | [c0, c1] `elem` twoCharOps = (lineNum, [c0, c1]) : lex cs lineNum
lex (c : cs) lineNum = (lineNum, [c]) : lex cs lineNum