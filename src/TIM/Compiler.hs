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
  if n == 0 && usedSlots == 0
    then (name, is)
    else (name, Take usedSlots n : is)
  where
    n = length paraNames
    (usedSlots, is) = compileR body (zipWith (\name i -> (name, Arg i)) paraNames [1 ..] ++ env) n

compileR :: CoreExpr -> TimEnvironment -> Int -> (Int, TimCode)
compileR e@(Num {}) env usedSlots = compileB e env usedSlots [Return]
compileR e@(Application (Var "negate") _) env usedSlots = compileB e env usedSlots [Return]
compileR e@(Application (Application (Var op) e1) e2) env usedSlots
  | op `elem` domain primitiveOpMap = compileB e env usedSlots [Return]
compileR (Let False defs exp) env usedSlots =
  (usedSlots'', zipWith Move indexs addrs ++ is)
  where
    n = length defs
    indexs = [usedSlots + 1 .. usedSlots + n]
    (usedSlots', addrs) = seqCompile False compileA (map snd defs) env (usedSlots + n)
    env' = zipWith (\n i -> (n, Arg i)) (domain defs) indexs ++ env
    (usedSlots'', is) = compileR exp env' usedSlots'
compileR (Let True defs exp) env usedSlots =
  (usedSlots'', zipWith Move indexs addrs ++ is)
  where
    n = length defs
    indexs = [usedSlots + 1 .. usedSlots + n]
    env' = zipWith (\n i -> (n, makeIndirectMode i)) (domain defs) indexs ++ env
    (usedSlots', addrs) = seqCompile False compileA (map snd defs) env' (usedSlots + n)
    (usedSlots'', is) = compileR exp env' usedSlots'
compileR (Application (Application (Application (Var "if") e1) e2) e3) env usedSlots =
  compileB e1 env slots [Cond (head codes) (codes !! 1)]
  where
    (slots, codes) = seqCompile True compileR [e2, e3] env usedSlots
compileR (Application e1 e2) env usedSlots = (max slots1 slots2, Push addr : is)
  where
    (slots1, addr) = compileA e2 env usedSlots
    (slots2, is) = compileR e1 env usedSlots
compileR e@(Var {}) env usedSlots = (slots, [Enter addr])
  where (slots, addr) = compileA e env usedSlots
compileR e env usedSlots = error ("compileR: cannot compile " ++ show e)

seqCompile :: Bool -> (CoreExpr -> TimEnvironment -> Int -> (Int, a)) -> [CoreExpr] -> TimEnvironment -> Int -> (Int, [a])
seqCompile slotShared compile exps env usedSlots =
  if slotShared
    then mapAccumL (\a b -> --这也不对，shared 的时候都是从 usedSlots 开始
      let (a', code) = compile b env usedSlots in
          (max a a', code))
      usedSlots
      exps
    else mapAccumL (\a b -> let (a', code) = compile b env a in (a', code)) usedSlots exps

makeIndirectMode :: Int -> TimAddrMode
makeIndirectMode n = Code [Enter (Arg n)]

compileA :: CoreExpr -> TimEnvironment -> Int -> (Int, TimAddrMode)
compileA (Var name) env usedSlots = (usedSlots, lookup env name (error ("Unknown variable: " ++ name)))
compileA (Num n) env usedSlots = (usedSlots, IntConst n)
compileA e env usedSlots = (slots, Code is)
  where (slots, is) = compileR e env usedSlots

type Continuation = TimCode
compileB :: CoreExpr -> TimEnvironment -> Int -> Continuation -> (Int, TimCode)
compileB e@(Application (Var "negate") e1) env usedSlots cont = compileB e1 env usedSlots (Op Neg : cont)
compileB (Application (Application (Var op) e1) e2) env usedSlots cont
  | op `elem` domain primitiveOpMap = do
    let (slots1, is1) = compileB e1 env usedSlots (Op (lookup primitiveOpMap op (error "impossible")) : cont)
    let (slots2, is2) = compileB e2 env usedSlots is1
    (max slots1 slots2, is2)
compileB (Num n) env usedSlots cont = (usedSlots, PushV (IntValueConst n) : cont)
compileB e env usedSlots cont =
  if null cont
    then compileR e env usedSlots
    else let (slots, is) = compileR e env usedSlots in (slots, Push (Code cont) : is)

primitiveOpMap :: [(Name, Op)]
primitiveOpMap = [
  ("+", Add),
  ("-", Sub),
  ("*", Mul),
  ("/", Div),
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


