module TIM.Compiler where

import AST (CoreProgram, CoreSuperCombinator, Name, CoreExpr, Expr (..), isFullApplication, Alter)
import TIM.Util (TimState (TimState), Instruction (..), TimAddrMode (..), FramePtr (FrameNull), initValueStack, initDump, initStats, TimCode, Op (..), Closure, ValueAddrMode (IntValueConst), initOutput, setupInitStack, allocateInitHeap, genTopCont, genHeadCont)
import Heap (lookup, initHeap)
import Prelude hiding (lookup)
import CorePrelude (primitives, defs)
import Util (domain)
import Data.List (mapAccumL)
import qualified Data.Bifunctor
import Debug.Trace (trace)

-- | Int is parameters count
type TimEnvironment = [(Name, (TimAddrMode, Maybe Int))]

compile :: CoreProgram -> TimState
compile program = TimState [Enter mainAddr] FrameNull FrameNull initStack initValueStack initDump initHeap codeStore initStats initOutput
  where
    -- scDefs = defs ++ primitives ++ program
    -- scDefs = defs ++ program
    scDefs = program
    initEnv = concat [makeEnvItem paras name (indexOf name) (indexOf (name ++ "_fullApp")) | (name, paras) <- map (\sc@(n, paras, _) -> (n, analyzePara sc)) scDefs]
    compiledScDefs = concatMap ((\x@(name, code) -> [x, (name ++ "_fullApp", removeUpdaters code)]) . (`compileSuperCombinator` initEnv)) scDefs
    (heap, codeStore@(_, namePosMap)) = allocateInitHeap ["topCont", "headCont"] (("topCont", topCont) : ("headCont", headCont) : compiledScDefs)
    (initStack, initHeap) = setupInitStack heap topCont 2
    mainAddr = makeLabel "main_fullApp"
    indexOf name = lookup namePosMap name (error ("not found name: " ++ name))
    makeLabel name = Label name (indexOf name)
    topCont = genTopCont (indexOf "headCont")
    headCont = genHeadCont (indexOf "topCont")

analyzePara :: CoreSuperCombinator -> Int
analyzePara (_, [], Constructor _ arity) = arity
analyzePara (_, paras, _) = length paras

-- 对于 cons = Pack{2, 2}\n func (cons 1) 
--这样的情况下面 name 为 full application 的优化会有问题，对于 cons，这里是个假 full application
makeEnvItem :: Int -> Name -> Int -> Int -> TimEnvironment
makeEnvItem paraCount name k fullAppK =
  if paraCount > 0
    then [(name, (Label name k, Just paraCount)),
      (fullAppName, (Label fullAppName fullAppK, Just paraCount))]
    else [(name, (Code [Enter (Label name k)], Just paraCount)),
      (fullAppName, (Code [Enter (Label fullAppName fullAppK)], Just paraCount))]
  where fullAppName = name ++ "_fullApp"
removeUpdaters :: TimCode -> TimCode
removeUpdaters ((UpdateMarkers _) : xs) = xs
removeUpdaters xs = xs

compileSuperCombinator :: CoreSuperCombinator -> TimEnvironment -> (Name, TimCode)
compileSuperCombinator (name, paraNames, body) env
  | usedSlots == 0 = (name, is)
  | paraCount == 0 = (name, Take usedSlots paraCount : is)
  | otherwise = (name, UpdateMarkers paraCount : Take usedSlots paraCount : is)
  where
      paraCount = length paraNames
      (usedSlots, is)
        = compileR
            body (zipWith (\name i -> (name, (Arg i, Nothing))) paraNames [1 .. ] ++ env)
            paraCount

data ArgStatus = Full | Partial | Unknown
queryParaCount :: TimEnvironment -> Name -> Maybe Int
queryParaCount env name = snd (lookup env name (error ("Unknown variable: " ++ name ++ " in " ++ show (length env))))

compileR :: CoreExpr -> TimEnvironment -> Int -> (Int, TimCode)
compileR e@(Application e1 e2) env =
  inCompileR
    e
    (if isFullApplication e1 1 (queryParaCount env) then Full else Partial)
    env
compileR e@(Let {}) env =
  inCompileR
    e
    (if isFullApplication e 0 (queryParaCount env) then Full else Partial)
    env
compileR e env = inCompileR e Unknown env

-- + if 这些可以 partial application 吗？可以，下面有 compiledPrimitives，外面有 primitives
inCompileR :: CoreExpr -> ArgStatus -> TimEnvironment -> Int -> (Int, TimCode)
inCompileR e@(Num {}) _ env usedSlots = compileB e env usedSlots [Return]
inCompileR (Constructor tag arity) _ env usedSlots =
  if arity == 0
    then (usedSlots, [ReturnConstructor tag])
    else (usedSlots, [UpdateMarkers arity, Take arity arity, ReturnConstructor tag])
inCompileR (Case e alts) _ env usedSlots = (slots2, Push (Code [Switch branches]) : is) -- TODO chanage
  where
    (slots1, branches) = seqCompile True compileE alts env usedSlots
    (slots2, is) = compileR e env slots1
inCompileR e@(Application (Var "negate") _) _ env usedSlots = compileB e env usedSlots [Return]
inCompileR e@(Application (Application (Var op) e1) e2) _ env usedSlots
  | op `elem` domain primitiveOpMap = compileB e env usedSlots [Return]
inCompileR (Let False defs exp) argStat env usedSlots =
  (usedSlots'', zipWith Move indexs addrs ++ is)
  where
    n = length defs
    indexs = [usedSlots + 1 .. usedSlots + n]
    (usedSlots', addrs) = seqCompile False compileU (zip (map snd defs) indexs) env (usedSlots + n)
    env' = zipWith (\n i -> (n, (makeIndirectMode i, Nothing))) (domain defs) indexs ++ env
    (usedSlots'', is) = inCompileR exp argStat env' usedSlots' -- let 这里要怎么编呢？
inCompileR (Let True defs exp) argStat env usedSlots =
  (usedSlots'', zipWith Move indexs addrs ++ is)
  where
    n = length defs
    indexs = [usedSlots + 1 .. usedSlots + n]
    env' = zipWith (\n i -> (n, (makeIndirectMode i, Nothing))) (domain defs) indexs ++ env
    (usedSlots', addrs) = seqCompile False compileU (zip (map snd defs) indexs) env' (usedSlots + n)
    (usedSlots'', is) = inCompileR exp argStat env' usedSlots'
inCompileR (Application (Application (Application (Var "if") e1) e2) e3) _ env usedSlots =
  compileB e1 env slots [Cond (head codes) (codes !! 1)]
  where
    (slots, codes) = seqCompile True compileR [e2, e3] env usedSlots
inCompileR (Application e v@(Var {})) argStat env usedSlots = (slots, Push (compileA v env) : is)
  where (slots, is) = inCompileR e argStat env usedSlots
inCompileR (Application e n@(Num {})) argStat env usedSlots = (slots, Push (compileA n env) : is)
  where (slots, is) = inCompileR e argStat env usedSlots
inCompileR e@(Application e1 e2) argStat env usedSlots =
  (slots2, Move argIndex addr : Push (makeIndirectMode argIndex) : is)
  where
    (slots1, addr) = compileU (e2, argIndex) env argIndex
    (slots2, is) = inCompileR e1 argStat env slots1
    argIndex = usedSlots + 1
inCompileR e@(Var n) Full env usedSlots = (usedSlots, makeEnter (compileA (Var (n ++ "_fullApp")) env))
inCompileR e@(Var {}) _ env usedSlots = (usedSlots, makeEnter (compileA e env))
inCompileR e _ env usedSlots = error ("compileR: cannot compile " ++ show e)

seqCompile :: Bool -> (b -> TimEnvironment -> Int -> (Int, a)) -> [b] -> TimEnvironment -> Int -> (Int, [a])
seqCompile slotShared compile exps env usedSlots =
  if slotShared
    then mapAccumL (\a b ->
      let (a', code) = compile b env usedSlots in
          (max a a', code))
      usedSlots
      exps
    else mapAccumL (\a b -> let (a', code) = compile b env a in (a', code)) usedSlots exps

makeIndirectMode :: Int -> TimAddrMode
makeIndirectMode n = Code [Enter (Arg n)]
makeUpdateIndirectMode :: Int -> TimAddrMode
makeUpdateIndirectMode n = Code [PushMarker n, Enter (Arg n)]
makeEnter :: TimAddrMode -> [Instruction]
makeEnter (Code i) = i
makeEnter addr = [Enter addr]

compileA :: CoreExpr -> TimEnvironment -> TimAddrMode
compileA (Var name) env = fst $ lookup env name (error ("Unknown variable: " ++ name))
compileA (Num n) env = IntConst n

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

compileU :: (CoreExpr, Int) -> TimEnvironment -> Int -> (Int, TimAddrMode)
compileU (Num n, slot) env usedSlots = (usedSlots, IntConst n)
compileU (e, slot) env usedSlots = (slots', Code (PushMarker slot : is))
  where (slots', is) = compileR e env usedSlots

compileE :: Alter Name -> TimEnvironment -> Int -> (Int, (Int, TimCode))
compileE (tag, paras, body) env usedSlots = (slots, (tag, moves ++ is))
  where
    n = length paras
    moves = map (\i -> Move (usedSlots + i) (Data i)) [1 .. n]
    env' = zipWith (\n i -> (n, (Arg i, Nothing))) paras [usedSlots + 1 .. usedSlots + n] ++ env
    (slots, is) = compileR body env' (usedSlots + n)

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
    UpdateMarkers 2,
    Take 2 2,
    Push (Code [
      Push (Code [Op Add, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("-", [
    UpdateMarkers 2,
    Take 2 2,
    Push (Code [
      Push (Code [Op Sub, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("*", [
    UpdateMarkers 2,
    Take 2 2,
    Push (Code [
      Push (Code [Op Mul, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("/", [
    UpdateMarkers 2,
    Take 2 2,
    Push (Code [
      Push (Code [Op Div, Return]),
      Enter (Arg 1)]),
    Enter (Arg 2)]),
  ("negate", [
    UpdateMarkers 1,
    Take 1 1,
    Push (Code [Op Neg, Return]),
    Enter (Arg 1)
    ]),
  ("if", [
    UpdateMarkers 3,
    Take 3 3,
    Push (Code [
      Cond [Enter (Arg 2)] [Enter (Arg 3)]]), -- 这个不用加 Return 吗？
    Enter (Arg 1)
    ])
  ]
