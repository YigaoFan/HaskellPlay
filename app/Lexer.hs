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
import Data.List (isSuffixOf)

type Token = (Int, String) -- String in token is never empty

isIdChar :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_') || (c == '\'')

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]

spanIf :: ([a] -> a -> Bool) -> [a] -> ([a], [a])
spanIf pred xs =
  iter [] xs
  where
    iter okPart xs@(x : remain) =
      if pred okPart x
      then iter (okPart ++ [x]) remain
      else (okPart, xs)
    iter okPart [] = (okPart, [])

lex :: String -> Int -> [Token]
lex cs lineNum
  | cs == "" = []
lex (c : cs) lineNum
  | c == '"' = (lineNum, c : strAfterQuote) : lex remain lineNum
  where --Parser 里形成最终的 string 的时候要去掉转义符号
    (strAfterQuote, remain) = spanIf ok cs
    ok base x | not ("\"" `isSuffixOf` base) = True
    ok base x | "\\\"" `isSuffixOf` base = True
    ok _ _ = False
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
  | isAlpha c || c == '_' = (lineNum, c : varAfterFstChar) : lex rest lineNum
  where
    (varAfterFstChar, rest) = span isIdChar cs
    -- var = c : takeWhile isIdChar cs -- 严格来说一个 id 中，首字母和其余字母的要求是不一样的
    -- restChars = dropWhile isIdChar cs
lex (c0 : c1 : cs) lineNum -- last line in file which has comment will pass this item?
  | c0 == '|' && c1 == '|' = lex remain lineNum
  where
    remain = dropWhile ('\n' /=) cs
lex (c0 : c1 : cs) lineNum
  | [c0, c1] `elem` twoCharOps = (lineNum, [c0, c1]) : lex cs lineNum
lex (c : cs) lineNum = (lineNum, [c]) : lex cs lineNum