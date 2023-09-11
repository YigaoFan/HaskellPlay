-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module PrettyPrint where

import AST (Alter, CoreExpr, CoreProgram, Expr (..), Name, isAtomicExpr)
import Control.Category ((.))
import Data.Bool (otherwise)
import Data.Foldable (Foldable (foldl))
import Data.List (concat, intercalate)
import Data.String (String, unwords)
import GHC.Base (Int, map, (++))
import GHC.List (take)
import GHC.Show (Show (show))
import Prelude hiding(print, (.))

class PrettyPrinted a where
  print :: a -> String

instance Show a => PrettyPrinted (Expr a) where
  print :: Expr a -> String
  print (Num n) = show n
  print (Var v) = v
  print (Application e0 e1) = print e0 ++ " " ++ print e1
  print (Constructor tag arity) = "Pack{" ++ show tag ++ ", " ++ show arity ++ "}"
  print (Let isRecursive defs e) =
    if isRecursive
      then "letrec " ++ printDefs defs ++ " in " ++ print e
      else "let " ++ printDefs defs ++ " in " ++ print e
  print (Case e alts) = "case " ++ print e ++ " of " ++ printAlts alts
  print (Lambda paras e) = "\\" ++ printParas paras ++ ". " ++ print e

printDefs :: Show a => [(a, Expr a)] -> String
printDefs = intercalate "; " . map (\(a, e) -> show a ++ " = " ++ show e)
printWithParen :: Show a => Expr a -> String
printWithParen e
  | isAtomicExpr e = print e
  | otherwise = "(" ++ print e ++ ")"

printAlts :: Show a => [Alter a] -> String
printAlts = intercalate "; " . map (\(n, vars, e) -> "<" ++ show n ++ "> " ++ unwords (map show vars) ++ " -> " ++ show e)

printParas :: Show a => [a] -> String
printParas = unwords . map show

makeMultiApplication :: Int -> CoreExpr -> CoreExpr -> CoreExpr
makeMultiApplication n e0 e1 = foldl Application e0 (take n e1s) where e1s = e1 : e1s

data Sequence = Nil
  | Str String
  | Append Sequence Sequence
  | Newline
  | Indent Sequence

display :: Sequence -> String
