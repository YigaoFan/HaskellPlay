-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module PrettyPrint where

import AST (Alter, CoreExpr, CoreProgram, Expr (..), Name, isAtomicExpr)
import Control.Category ((.))
import Data.Bool (otherwise)
import Data.Foldable (Foldable (foldl))
import Data.List ( intercalate)
import Data.String (String, unwords)
import GHC.Base (Int, map, (++))
import GHC.List (take)
import GHC.Show (Show (show), showSpace)
import Prelude hiding(concat, print, (.))
import Heap (Addr)

class PrettyPrinted a where
  print :: a -> Int -> Sequence

instance Show a => PrettyPrinted (Expr a) where
  print :: Expr a -> Int -> Sequence
  print (Num n) _ = Str (show n)
  print (Var v) _ = Str v

  print (Application (Application (Var "|") e0) e1) precedence =
    addParenIfNeed precedence 1 (concat [print e0 1, Str " | ", print e1 1])
  print (Application (Application (Var "&") e0) e1) precedence =
    addParenIfNeed precedence 2 (concat [print e0 2, Str " & ", print e1 2])
  print (Application (Application (Var "==") e0) e1) precedence =
    addParenIfNeed precedence 3 (concat [print e0 3, Str " == ", print e1 3])
  print (Application (Application (Var "~=") e0) e1) precedence =
    addParenIfNeed precedence 3 (concat [print e0 3, Str " ~= ", print e1 3])
  print (Application (Application (Var ">") e0) e1) precedence =
    addParenIfNeed precedence 3 (concat [print e0 3, Str " > ", print e1 3])
  print (Application (Application (Var ">=") e0) e1) precedence =
    addParenIfNeed precedence 3 (concat [print e0 3, Str " >= ", print e1 3])
  print (Application (Application (Var "<") e0) e1) precedence =
    addParenIfNeed precedence 3 (concat [print e0 3, Str " < ", print e1 3])
  print (Application (Application (Var "<=") e0) e1) precedence =
    addParenIfNeed precedence 3 (concat [print e0 3, Str " <= ", print e1 3])
  print (Application (Application (Var "-") e0) e1) precedence = 
    addParenIfNeed precedence 4 (concat [print e0 4, Str " - ", print e1 4])
  print (Application (Application (Var "+") e0) e1) precedence = 
     addParenIfNeed precedence 4 (concat [print e0 4, Str " + ", print e1 4])
  print (Application (Application (Var "*") e0) e1) precedence = 
    addParenIfNeed precedence 5 (concat [print e0 5, Str " * ", print e1 5])
  print (Application (Application (Var "/") e0) e1) precedence = 
    addParenIfNeed precedence 5 (concat [print e0 5, Str " / ", print e1 5])
  print (Application e0 e1) precedence = addParenIfNeed precedence 6 (concat [print e0 6, Str " ", print e1 6])
  print (Constructor tag arity) precedence = addParenIfNeed precedence 6 (concat [Str "Pack{", Str (show tag), Str ", ", Str (show arity), Str "}"])
  
  -- 下面这几个组合起来，优先级的事情怎么弄 mark it to 0 to expect inner to add paren
  print (Let isRecursive defs e) precedence =
    addParenIfNeed precedence 0 (
      if isRecursive
        then concat [Str "letrec ", printDefs defs, Str " in ", print e 0]
        else concat [Str "let ", printDefs defs, Str " in ", print e 0])
  print (Case e alts) precedence = addParenIfNeed precedence 0 (concat [Str "case ", print e 0, Str " of ", printAlts alts])
  print (Lambda paras e) precedence = addParenIfNeed precedence 0 (concat [Str "\\", printParas paras, Str ". ", print e 0])

printDefs :: Show a => [(a, Expr a)] -> Sequence
printDefs = interleave (Str "; ") . map (\(a, e) -> concat [Str (show a), Str " = ", print e 0])
addParenIfNeed :: Int -> Int -> Sequence -> Sequence
addParenIfNeed outPrecedence currentPrecedence seq =
  -- 更细致点的话，左边的优先级也比右边优先级高，比如 1 - 2 + 3 和 1 - 2 + (1 - 2)，这里是怎么组成 AST 的？这里的代码就会给第一个的左边减法加上括号，其实不需要
  if currentPrecedence > outPrecedence 
    then seq
    else concat [Str "(", seq, Str ")"]

printAlts :: Show a => [Alter a] -> Sequence
printAlts = interleave (Str "; ") . map (\(n, vars, e) -> concat [Str "<", Str (show n), Str "> ", Str (unwords (map show vars)), Str " -> ", print e 0])

printParas :: Show a => [a] -> Sequence
printParas = interleave (Str " ") . map (Str . show)

makeMultiApplication :: Int -> CoreExpr -> CoreExpr -> CoreExpr
makeMultiApplication n e0 e1 = foldl Application e0 (take n e1s) where e1s = e1 : e1s

makeSentence = concat [str "hi meng,\n", Indent (str "how are you"), Newline, str "Best regards"]

makeCalculate :: Expr String
makeCalculate = Application (Application (Var "*") (Num 2)) (Application (Application (Var "+") (Num 3)) (Num 4))

data Sequence = Nil
  | Str String
  | Append Sequence Sequence
  | Newline
  | Indent Sequence

replace :: Char -> Sequence -> String -> Sequence
replace c seq (c0 : s) | c == c0 = Append seq (replace c seq s)
replace c seq [] = Nil
replace c seq s = Append (Str l) (replace c seq r) where (l, r) = span (/=c) s
str :: String -> Sequence
str = replace '\n' Newline
concat :: [Sequence] -> Sequence
concat = foldl Append Nil
interleave :: Sequence -> [Sequence] -> Sequence
interleave sep =
  foldl (\b s ->
    case b of
      Nil -> s
      _   -> concat [b, sep, s]) Nil
space :: Int -> String
space n = replicate n ' '

num :: Int -> Sequence
num = Str . show

fillSpaceNum :: Int -> Int -> Sequence
fillSpaceNum width n =
  Str (space (width - length digits) ++ digits)
  where
    digits = show n
  
layn :: [Sequence] -> Sequence
layn seqs = concat (map (\(n, seq) -> concat [fillSpaceNum 4 n, Str ")", Indent seq, Newline]) (zip [1..] seqs))

flatten :: Int -> [(Sequence, Int)] -> String
flatten _ [] = ""
flatten col ((Nil, indent) : seqs) = flatten col seqs
flatten col ((Str s, indent) : seqs) = space indent ++ s ++ flatten (col+indent+length s) seqs
flatten col ((Append s0 s1, indent) : seqs) = flatten col ((s0, indent) : (s1, 0) : seqs)
flatten col ((Newline, indent) : seqs) = '\n' : space indent ++ flatten indent seqs
flatten col ((Indent s, indent) : seqs) = flatten col ((s, 2) : seqs)

-- 耗时等比于 s 的长度
display :: Sequence -> String
display s = flatten 0 [(s, 0)]

showAddr :: Addr -> Sequence
showAddr = Str . show