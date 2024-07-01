module CorePrelude where

import AST (CoreProgram, Expr(..), CoreSuperCombinator)
import Parser (parse)

primitives :: [CoreSuperCombinator]
primitives = [
  ("negate", ["x"], Application (Var "negate") (Var "x")),
  ("+", ["x", "y"], Application (Application (Var "+") (Var "x")) (Var "y")),
  ("-", ["x", "y"], Application (Application (Var "-") (Var "x")) (Var "y")),
  ("*", ["x", "y"], Application (Application (Var "*") (Var "x")) (Var "y")),
  ("/", ["x", "y"], Application (Application (Var "/") (Var "x")) (Var "y")),
  ("==", ["x", "y"], Application (Application (Var "==") (Var "x")) (Var "y")),
  ("/=", ["x", "y"], Application (Application (Var "/=") (Var "x")) (Var "y")),
  (">=", ["x", "y"], Application (Application (Var ">=") (Var "x")) (Var "y")),
  (">", ["x", "y"], Application (Application (Var ">") (Var "x")) (Var "y")),
  ("<=", ["x", "y"], Application (Application (Var "<=") (Var "x")) (Var "y")),
  ("<", ["x", "y"], Application (Application (Var "<") (Var "x")) (Var "y")),
  ("if", ["c", "t", "f"], Application (Application (Application (Var "if") (Var "c")) (Var "t")) (Var "f")),
  ("True", [], Constructor 2 0),
  ("False", [], Constructor 1 0)
  ]
-- 这几个定义的是函数还是类型构造器？
defs :: CoreProgram
defs 
  = [
    -- ("id", ["x"], Var "x"),
    ("left", ["x", "y"], Var "x"), -- K
    ("right", ["x", "y"], Var "y"), -- K1
    ("s", ["f", "g", "x"], Application (Application (Var "f") (Var "x")) -- f x (g x)
                                       (Application (Var "g") (Var "x"))),
    ("compose", ["f", "g", "x"], Application (Var "f") (Application (Var "g") (Var "x"))),
    ("twice", ["f"], Application (Application (Var "compose") (Var "f")) (Var "f"))
  ]

extraDefs :: CoreProgram
extraDefs = parse
  "length xs = caseList xs 0 len\n\
  \len x xs = 1 + length xs\n\
  \head xs = caseList xs (abort \"empty list\") headHandleNonEmtpy\n\
  \headHandleNonEmtpy i is = i\n\
  \tail xs = caseList xs (abort \"empty list\") tailHandleNonEmtpy\n\
  \tailHandleNonEmtpy i is = is\n\
  \Nil = Pack{1, 0}\n\
  \Cons = Pack{2, 2}\n\
  \_main = printList main\n\
  \printList xs = caseList xs stop printCons\n\
  \printCons h t = print h (printList t)\n\
  \True = Pack{2, 0}\n\
  \False = Pack{1, 0}"