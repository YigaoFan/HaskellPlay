module CorePrelude where

import AST (CoreProgram, Expr(..))

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