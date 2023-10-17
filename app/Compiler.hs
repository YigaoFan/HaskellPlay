module Compiler where
import AST (CoreExpr, CoreSuperCombinator, Name)
import CorePrelude (defs)
import Heap
import Prelude hiding (lookup)


type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
data TiDump = DummyTiDump
initTiDump = DummyTiDump
type TiHeap = Heap Node
data Node = Application Addr Addr
  | SuperCombinator Name [Name] CoreExpr
  | Num Int
  | IndirectNode Addr
isDataNode :: Node -> Bool
isDataNode (Num _) = True
isDataNode _       = False
type Assoc a b = [(a, b)]
type TiGlobals = Assoc Name Addr
type TiStats = Int
initTiStats :: TiStats
initTiStats = 0
incTiStatSteps s = s + 1
getTiStatSteps s = s
applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f (s, d, h, g, stats) = (s, d, h, g, f stats)

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

buildInitHeap :: [CoreSuperCombinator] -> (TiHeap, TiGlobals)
buildInitHeap = mapAccum allocSuperCombinator initHeap
compile :: [CoreSuperCombinator] -> TiState
compile program =
  (initStack, initTiDump, initHeap, globals, initTiStats)
  where
    scDefs = program ++ defs
    (initHeap, globals) = buildInitHeap scDefs
    initStack = [addrOfMain]
    addrOfMain = lookup globals "main" (error "main is not defined")
-- 得写记录了
