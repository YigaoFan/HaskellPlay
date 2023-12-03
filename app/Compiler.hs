module Compiler where
import AST (CoreExpr, CoreSuperCombinator, Name)
import CorePrelude (defs, extraDefs)
import Heap
import Prelude hiding (lookup)
import Data.List (intercalate)

type TiState = (TiOutput, TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiOutput = [Int]
type TiStack = [Addr]
type TiDump = [TiStack]

initOutput :: TiOutput
initOutput = []
append :: TiOutput -> Int -> TiOutput
append output s = s : output
initTiDump = []
type TiHeap = Heap Node
data Primitive =
  Neg | Add | Sub | Mul | Div |
  Construct Int Int | If | Greater | GreaterEq |
  Less | LessEq | Eq | NotEq | CasePair | CaseList |
  Abort | Stop | Print
data Node = Application Addr Addr
  | SuperCombinator Name [Name] CoreExpr
  | Num Int
  | String String
  | IndirectNode Addr
  | Prim Name Primitive
  | Data Int [Addr]
isDataNode :: Node -> Bool
isDataNode (Num _) = True
isDataNode (Data _ _) = True
isDataNode (String _) = True
isDataNode _       = False
type Assoc a b = [(a, b)]
type TiGlobals = Assoc Name Addr
type TiStats = Int
initTiStats :: TiStats
initTiStats = 0
incTiStatSteps s = s + 1
getTiStatSteps s = s
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (o, s, d, h, g, stats) = (o, s, d, h, g, f stats)

mapAccum f acc (x : xs) = (newAcc, y : ys)
  where
    (acc', y) = f acc x
    (newAcc, ys) = mapAccum f acc' xs
mapAccum f acc [] = (acc, [])

allocSuperCombinator :: TiHeap -> CoreSuperCombinator -> (TiHeap, (Name, Addr))
allocSuperCombinator heap (name, args, body) =
  (heap', (name, addr))
  where
    (heap', addr) = heapAlloc heap (SuperCombinator name args body)
heapAddrs :: Heap a -> [Addr]
heapAddrs (_, _, addrObjs) = map fst addrObjs

allocatePrim :: TiHeap -> (Name, Primitive) -> (TiHeap, (Name, Addr))
allocatePrim heap (name, primitive) =
  let (heap', addr) = heapAlloc heap (Prim name primitive)
  in
    (heap', (name, addr))
primitives :: Assoc Name Primitive
primitives = [
  ("negate", Neg),
  ("+", Add),
  ("-", Sub),
  ("*", Mul),
  ("/", Div),
  ("if", If),
  (">", Greater),
  (">=", GreaterEq),
  ("<", Less),
  ("<=", LessEq),
  ("==", Eq),
  ("!=", NotEq),
  ("casePair", CasePair),
  ("caseList", CaseList),
  ("abort", Abort),
  ("stop", Stop),
  ("print", Print)
  ]
buildInitHeap :: [CoreSuperCombinator] -> (TiHeap, TiGlobals)
buildInitHeap scDefs = (heap2, scAddrs ++ primAddrs)
  where
    (heap1, scAddrs) = mapAccum allocSuperCombinator initHeap scDefs
    (heap2, primAddrs) = mapAccum allocatePrim heap1 primitives
compile :: [CoreSuperCombinator] -> TiState
compile program =
  (initOutput, initStack, initTiDump, initHeap, globals, initTiStats)
  where
    scDefs = program ++ defs ++ extraDefs
    (initHeap, globals) = buildInitHeap scDefs
    initStack = [addrOfMain]
    addrOfMain = lookup globals "_main" (error ("_main is not defined in [" ++ intercalate ", " (map fst globals) ++ "]"))
-- 得写记录了
