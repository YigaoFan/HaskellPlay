module CorePrelude where

import AST (CoreProgram, Expr(..))
import Parser (parse)

-- 这几个定义的是函数还是类型构造器？
defs :: CoreProgram
defs 
  = [
    ("id", ["x"], Var "x"),
    ("left", ["x", "y"], Var "x"),
    ("right", ["x", "y"], Var "y"),
    ("s", ["f", "g", "x"], Application (Application (Var "f") (Var "x"))
                                       (Application (Var "g") (Var "x"))),
    ("compose", ["f", "g", "x"], Application (Var "f") (Application (Var "g") (Var "x"))),
    ("twice", ["f"], Application (Application (Var "compose") (Var "f")) (Var "f"))
  ]

extraDefs :: CoreProgram
extraDefs = parse
  "length xs = caseList xs 0 len;\n\
  \len x xs = 1 + length xs;\n\
  \head xs = caseList xs (abort \"empty list\") headHandleNonEmtpy;\n\
  \headHandleNonEmtpy i is = i;\n\
  \tail xs = caseList xs (abort \"empty list\") tailHandleNonEmtpy;\n\
  \tailHandleNonEmtpy i is = is;\n\
  \Nil = Pack{1, 0};\n\
  \Cons = Pack{2, 2};\n\
  \_main = printList main;\n\
  \printList xs = caseList xs stop printCons;\n\
  \printCons h t = print h (printList t);"