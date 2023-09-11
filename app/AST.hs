{-# LANGUAGE GADTs #-}
module AST where

import Data.String (String)
import Data.Bool (Bool (True, False))
import GHC.Int (Int)
import GHC.Show (Show)

type Name = String
type IsRecursive = Bool
recursive :: IsRecursive
recursive = True
nonRecursive :: IsRecursive
nonRecursive = False
type Alter a = (Int, [a], Expr a)

data Expr a where
  Var :: Name -> Expr a
  Num :: Int -> Expr a
  Constructor :: Int -> Int -> Expr a
  Application :: (Expr a) -> (Expr a) -> Expr a
  Let :: IsRecursive -> [(a, Expr a)] -> (Expr a) -> Expr a
  Case :: (Expr a) -> [Alter a] -> Expr a
  Lambda :: [a] -> (Expr a) -> Expr a
  deriving (Show)

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (Var _) = True
isAtomicExpr (Num _) = True
isAtomicExpr _       = False

type CoreExpr = Expr Name
type SuperCombinator a = (Name, [a], Expr a)
type CoreSuperCombinator = SuperCombinator Name
type Program a = [SuperCombinator a]
type CoreProgram = Program Name