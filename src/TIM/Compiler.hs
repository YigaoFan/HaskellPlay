module TIM.Compiler where

import AST (CoreProgram, CoreSuperCombinator, Name, CoreExpr, Expr (..))
import TIM.Util (TimState (TimState), Instruction (..), TimAddrMode (..), FramePtr (FrameNull), initStack, initValueStack, initDump, initStats, TimCode, Op (..), Closure, ValueAddrMode (IntValueConst))
import Heap (lookup, initHeap)
import Prelude hiding (lookup)
import CorePrelude (primitives, defs)
import Util (domain)
import Data.List (mapAccumL)

type TimEnvironment = [(Name, TimAddrMode)]

compile :: CoreProgram -> TimState
compile program = TimState [Enter (Label "main")] FrameNull initStack initValueStack initDump initHeap compiledScDefs initStats
  where
    -- scDefs = defs ++ primitives ++ program
    scDefs = defs ++ program
    initEnv = [(n, Label n) | n <- map (\(n, _, _) -> n) scDefs]
    compiledScDefs = map (`compileSuperCombinator` initEnv) scDefs
    names = map (\(n, _, _) -> n) program

compileSuperCombinator :: CoreSuperCombinator -> TimEnvironment -> (Name, TimCode)
compileSuperCombinator (name, paraNames, body) env =
  if n == 0 && slotUsedCount == 0
    then (name, is)
    else (name, Take slotUsedCount n : is)
  where
    n = length paraNames
    (slotUsedCount, is) = compileR body (zipWith (\name i -> (name, Arg i)) paraNames [1 ..] ++ env) n

compileR :: CoreExpr -> TimEnvironment -> Int -> (Int, TimCode)
compileR e@(Num {}) env slotUsedCount = compileB e env slotUsedCount [Return]
compileR e@(Application (Var "negate") _) env slotUsedCount = compileB e env slotUsedCount [Return]
compileR e@(Application (Application (Var op) e1) e2) env slotUsedCount
  | op `elem` domain primitiveOpMap = compileB e env slotUsedCount [Return]
-- handle slot
compileR (Let False defs exp) env slotUsedCount =
  (slotUsedCount'', zipWith Move indexs addrs ++ is)
  where
    n = length defs
    indexs = [slotUsedCount + 1 .. slotUsedCount + n]
    (slotUsedCount', addrs) = mapAccumL (\a b -> let (a', addr) = compileA b env a in (a', addr)) (slotUsedCount + n) (map snd defs)
    env' = zipWith (\n i -> (n, Arg i)) (domain defs) indexs
    (slotUsedCount'', is) = compileR exp env' slotUsedCount'

compileR (Application (Application (Application (Var "if") e1) e2) e3) env slotUsedCount =
  compileB e1 env slot [Cond (head codes) (codes !! 1)]
  where
    (slot, codes) = compileExps [e3, e2] env slotUsedCount -- 这里是不是先给 e1 让地？不用让，运行时的地在编译时已经确定
compileR (Application e1 e2) env slotUsedCount = (slot2, Push addr : is)
  where
    (slot1, addr) = compileA e2 env slotUsedCount
    (slot2, is) = compileR e1 env slot1
compileR e@(Var {}) env slotUsedCount = (slot, [Enter addr])
  where (slot, addr) = compileA e env slotUsedCount
compileR e env slotUsedCount = error ("compileR: cannot compile " ++ show e)

compileExps :: [CoreExpr] -> TimEnvironment -> Int -> (Int, [TimCode])
compileExps exps env slotUsedCount =
  mapAccumL (\a b -> let (a', code) = compileR b env a in (a', code)) slotUsedCount exps

compileA :: CoreExpr -> TimEnvironment -> Int -> (Int, TimAddrMode)
compileA (Var name) env slotUsedCount = (slotUsedCount, lookup env name (error ("Unknown variable: " ++ name)))
compileA (Num n) env slotUsedCount = (slotUsedCount, IntConst n)
compileA e env slotUsedCount = (slot, Code is)
  where (slot, is) = compileR e env slotUsedCount

type Continuation = TimCode
compileB :: CoreExpr -> TimEnvironment -> Int -> Continuation -> (Int, TimCode)
compileB e@(Application (Var "negate") e1) env slotUsedCount cont = compileB e1 env slotUsedCount (Op Neg : cont)
compileB (Application (Application (Var op) e1) e2) env slotUsedCount cont
  | op `elem` domain primitiveOpMap =
    let (slot1, is1) = compileB e1 env slotUsedCount (Op (lookup primitiveOpMap op (error "impossible")) : cont) in
      compileB e2 env slot1 is1
compileB (Num n) env slotUsedCount cont = (slotUsedCount, PushV (IntValueConst n) : cont)
compileB e env slotUsedCount cont =
  if null cont
    then compileR e env slotUsedCount
    else let (slot, is) = compileR e env slotUsedCount in (slot, Push (Code cont) : is)

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

-- not full application will call this，对吗？如果 f n = if n，别处可以 f 3 1 2 吗？
compiledPrimitives :: [(Name, TimCode)]
compiledPrimitives = [
  ("+", [
    Take 2 2,
    Push (Code [
      Push (Code [Op Add, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("-", [
    Take 2 2,
    Push (Code [
      Push (Code [Op Sub, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("*", [
    Take 2 2,
    Push (Code [
      Push (Code [Op Mul, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("/", [
    Take 2 2,
    Push (Code [
      Push (Code [Op Div, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("negate", [
    Take 1 1,
    Push (Code [Op Neg, Return]),
    Enter (Arg 1)
    ]),
  ("if", [
    Take 3 3,
    Push (Code [
      Cond [Enter (Arg 2)] [Enter (Arg 3)]]), -- 这个不用加 Return 吗？
    Enter (Arg 1)
    ])
  ]


