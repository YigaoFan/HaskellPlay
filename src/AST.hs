{-# LANGUAGE GADTs #-}
module AST where

import Data.String (String)
import Data.Bool (Bool (True, False))
import GHC.Int (Int)
import GHC.Show (Show)
import Debug.Trace (trace)
import Text.Printf (printf)

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
  String :: String -> Expr a
  Constructor :: Int -> Int -> Expr a
  Application :: (Expr a) -> (Expr a) -> Expr a -- function and arg
  Let :: IsRecursive -> [(a, Expr a)] -> (Expr a) -> Expr a
  Case :: (Expr a) -> [Alter a] -> Expr a
  Lambda :: [a] -> (Expr a) -> Expr a
  deriving (Show, Eq)

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (Var _) = True
isAtomicExpr (Num _) = True
isAtomicExpr _       = False

type CoreExpr = Expr Name
type SuperCombinator a = (Name, [a], Expr a)
type CoreSuperCombinator = SuperCombinator Name
type Program a = [SuperCombinator a]
type CoreProgram = Program Name

makeSuperCombinator :: Name -> [a] -> Expr a -> SuperCombinator a
makeSuperCombinator n as exp = (n, as, exp)

makeProgram :: [SuperCombinator a] -> Program a
makeProgram scs = scs

-- 不完全，因为一个函数调用可能包含好几个函数调用，这些都可以判断
isFullApplication :: Expr Name -> Int -> (Name -> Maybe Int) -> Bool
isFullApplication (Application e1 e2) foundArgs query = isFullApplication e1 (foundArgs + 1) query
-- sometimes Var is local variable which doesn't have parameter info, so add Maybe in sign
isFullApplication (Var name) foundArgs query =
  case query name of
    Just num -> (foundArgs >= num) -- && (num /= 0)  -- not handle CAF now
    _ -> False
isFullApplication (Lambda xs _) foundArgs query = foundArgs == length xs
isFullApplication (Constructor _ arity) foundArgs query = foundArgs == arity
isFullApplication (Let _ _ e) foundArgs query = isFullApplication e foundArgs query
isFullApplication _ _ _ = False

data X_
data Y_
data XY a where
  A :: Int -> XY a
  B :: Char -> XY a
  C :: Bool -> XY X_
type X = XY X_ -- Contains values built with constructors A, B, and C 
type Y = XY Y_