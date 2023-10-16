module Compiler where
import AST (CoreExpr, CoreSuperCombinator, Name)
import Prelude hiding (lookup)
import CorePrelude (defs)

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
type TiStack = [Addr]
type Addr = Int
data TiDump = DummyTiDump
initTiDump = DummyTiDump
type TiHeap = Heap Node
type Heap a = (Int, [Int], [(Int, a)])
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

lookup :: Eq k => [(k, v)] -> k -> v -> v
lookup [] k' defaultValue = defaultValue
lookup ((k, v) : xs) k' defaultValue | k == k' = v
                                     | k /= k' = lookup xs k' defaultValue

mapAccum f acc (x : xs) = (newAcc, y : ys)
  where
    (acc', y) = f acc x
    (newAcc, ys) = mapAccum f acc' xs
mapAccum f acc [] = (acc, [])
initHeap :: TiHeap
initHeap = (0, [1..], [])
heapLookup :: Heap a -> Addr -> a
heapLookup (_, _, addrObjs) addr = lookup addrObjs addr (error ("don't have the addr" ++ show addr))
heapAlloc :: Heap a -> a -> (Heap a, Addr)
heapAlloc (size, next : free, addrObjs) a = ((size + 1, free, (next, a) : addrObjs), next)
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
