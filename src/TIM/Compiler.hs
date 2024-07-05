module TIM.Compiler where

import AST (CoreProgram, CoreSuperCombinator, Name, CoreExpr, Expr (..))
import TIM.Util (TimState (TimState), Instruction (..), TimAddrMode (..), FramePtr (FrameNull), initStack, initValueStack, initDump, initStats, TimCode, Op (..), Closure, ValueAddrMode (IntValueConst))
import Heap (lookup, initHeap)
import Prelude hiding (lookup)
import CorePrelude (primitives, defs)
import Util (domain)

type TimEnvironment = [(Name, TimAddrMode)]

compile :: CoreProgram -> TimState
compile program = TimState [Enter (Label "main")] FrameNull initStack initValueStack initDump initHeap compiledScDefs initStats
  where
    -- scDefs = defs ++ primitives ++ program
    scDefs = defs ++ program
    initEnv = [(n, Label n) | n <- map fst compiledPrimitives ++ map (\(n, _, _) -> n) scDefs]
    compiledScDefs = compiledPrimitives ++ map (`compileSuperCombinator` initEnv) scDefs
    names = map (\(n, _, _) -> n) program

compileSuperCombinator :: CoreSuperCombinator -> TimEnvironment -> (Name, TimCode)
compileSuperCombinator (name, paraNames, body) env =
  if n == 0
    then (name, compileR body (zipWith (\name i -> (name, Arg i)) paraNames [1..] ++ env))
    else (name, Take n : compileR body (zipWith (\name i -> (name, Arg i)) paraNames [1..] ++ env))
  where n = length paraNames

compileR :: CoreExpr -> TimEnvironment -> TimCode
compileR e@(Application (Var "negate") _) env = compileB e env [Return]
compileR e@(Application (Application (Var op) e1) e2) env
  | op `elem` domain primitiveOpMap = compileB e env [Return]
compileR (Application (Application (Application (Var "if") e1) e2) e3) env =
  compileB e1 env [Cond (compileB e2 env [Return]) (compileB e3 env [Return])] -- 这里为什么又要加 Return 了？
compileR (Application e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR e@(Num {}) env = [Enter (compileA e env)]
compileR e@(Var {}) env = [Enter (compileA e env)]
compileR e env = error ("compileR: cannot compile " ++ show e)

compileA :: CoreExpr -> TimEnvironment -> TimAddrMode
compileA (Var name) env = lookup env name (error ("Unknown variable: " ++ name))
compileA (Num n) env = IntConst n
compileA e env = Code (compileR e env)

type Continuation = TimCode
compileB :: CoreExpr -> TimEnvironment -> Continuation -> TimCode
compileB e@(Application (Var "negate") e1) env cont = compileB e1 env (Op Neg : cont)
compileB (Application (Application (Var op) e1) e2) env cont
  | op `elem` domain primitiveOpMap =
    compileB e2 env
      (compileB e1 env
        (Op (lookup primitiveOpMap op (error "impossible")) : cont))
compileB (Num n) env cont = PushV (IntValueConst n) : cont
compileB e env cont = 
  if null cont
    then compileR e env
    else Push (Code cont) : compileR e env

primitiveOpMap :: [(Name, Op)]
primitiveOpMap = [
  ("+", Add),
  ("-", Sub),
  ("*", Mul),
  ("/", Div),
  -- ("negate", Neg), 不是双目运算
  (">=", GrEq),
  (">", Gr),
  ("<=", LtEq),
  ("<", Lt),
  ("==", Eq),
  ("/=", NotEq)
  ]
compiledPrimitives :: [(Name, TimCode)]
compiledPrimitives = [
  ("+", [
    Take 2,
    Push (Code [
      Push (Code [Op Add, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("-", [
    Take 2,
    Push (Code [
      Push (Code [Op Sub, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("*", [
    Take 2,
    Push (Code [
      Push (Code [Op Mul, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("/", [
    Take 2,
    Push (Code [
      Push (Code [Op Div, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("negate", [
    Take 1,
    Push (Code [Op Neg, Return]),
    Enter (Arg 1)
    ]),
  ("if", [
    Take 3,
    Push (Code [
      Cond [Enter (Arg 2)] [Enter (Arg 3)]]), -- 这个不用加 Return 吗？
    Enter (Arg 1)
    ])
  ]


