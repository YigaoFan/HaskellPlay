module MarkScanGC where
import Heap (Addr, heapUpdate, heapLookup, addrsOf, heapFree)
import Compiler (MarkState (Done, Visits), Node (Application, Data, IndirectNode, Marked, Prim, SuperCombinator, Num, String), TiDump, TiGlobals, TiHeap, TiStack, TiState)
import Debug.Trace (trace)
import Data.Maybe (Maybe)

findStackRoots :: TiStack -> [Addr]
findStackRoots stack = stack

findDumpRoots :: TiDump -> [Addr]
findDumpRoots dump = []

findGlobalRoots :: TiGlobals -> [Addr]
findGlobalRoots = foldr (\(_, addr) b -> addr : b) []

isMarkDone heap addr =
  case heapLookup heap addr of
    Marked Done n -> True
    _             -> False
isMarkVisit heap addr =
  case heapLookup heap addr of
    Marked (Visits _) n -> True
    _                   -> False
isApplication heap addr =
  case heapLookup heap addr of
    Application _ _ -> True
    _               -> False
isPrim heap addr =
  case heapLookup heap addr of
    Prim _ _ -> True
    _        -> False

isFinal :: (Maybe Addr, Maybe Addr, TiHeap) -> Maybe Node -> Maybe Node -> Bool
isFinal (Just _, Nothing, heap) (Marked Done _) _ = True
isFinal _ _ _ = False
-- So good to add these two args to let them can be pattern match

-- 需要为这里专门搞个 Application 类型来兼容不同的 a1 a2 类型
mark :: (Maybe Addr, Maybe Addr, TiHeap) -> Maybe Node -> Maybe Node -> (Maybe Addr, Maybe Addr, TiHeap)
mark (Just _, backward, heap) (IndirectNode a) _ = (Just a, backward, heap)
mark (forward@(Just f), backward, heap) (Just node@(Application a1 a2)) _ = (Just a1, forward, heapUpdate heap f (Marked (Visits 1 [backward]) node)) -- here backward type is not compatiable
mark (Just f, backward@(Just b), heap) (Just (Marked Done _)) (Just (Marked (Visits 1 ) (Application b' a2)))
  = (Just a2, backward, heapUpdate heap b (Marked (Visits 2) (Application f b')))
mark (Just f, backward@(Just b), heap) (Marked Done _) (Marked (Visits 2) (Application a1 b'))
  = (backward, b', heapUpdate heap b (Marked Done (Application a1 f)))
-- 不确定这个b'是不是有效地址
mark (forward@(Just f), backward, heap) (Just node@(Prim {})) _ = (forward, backward, heapUpdate heap f (Marked Done node))
mark (forward@(Just f), backward, heap) (Just node@(Num {})) _ = (forward, backward, heapUpdate heap f (Marked Done node))
mark (forward@(Just f), backward, heap) (Just node@(String {})) _ = (forward, backward, heapUpdate heap f (Marked Done node))
mark (forward@(Just f), backward, heap) (Just node@(SuperCombinator {})) _ = (forward, backward, heapUpdate heap f (Marked Done node))

-- TODO change
markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
markFrom heap addr = do
  let node = heapLookup heap addr
  case node of
    Marked _          -> (heap, addr)
    IndirectNode a    -> markFrom heap a
    _                 -> (markInner node (heapUpdate heap addr (Marked node)), addr)
  where
    markInner (Application a0 a1) h =
      let (newH, newAddrs) = markAddrs h [a0, a1] in
        heapUpdate newH addr (Marked (Application (head newAddrs) (newAddrs!!1)))
    markInner (Data t addrs) h =
      let (newH, newAddrs) = markAddrs h addrs
       in heapUpdate newH addr (Marked (Data t newAddrs))
    markInner _ h = h
    markAddrs h = foldr
      (\a (prevH, addrs) -> let (newH, newA) = markFrom prevH a in (newH, newA : addrs))
      (h, [])

scan :: TiHeap -> TiHeap
scan heap = do
  let addrs = addrsOf heap
  foldl (\h a ->
    case heapLookup h a of
      Marked node -> heapUpdate h a node
      _           -> heapFree h a
    )
    heap addrs

findRoots :: TiState -> [Addr]
findRoots (_, stack, dump, _, global, _) =
  findStackRoots stack ++
  findDumpRoots dump   ++
  findGlobalRoots global

gc :: TiState -> TiState
gc state@(output, stack, dump, heap, global, stats) = do
  let (heap1, newStack) = markFromStack heap stack
  let (heap2, newDump) = markFromDump heap1 dump
  let (heap3, newGlobals) = markFromGlobals heap2 global
  (output, newStack, newDump, scan heap3, newGlobals, stats)

markFromStack :: TiHeap -> TiStack -> (TiHeap, TiStack)
markFromStack heap = foldr (\a (h, newStack) -> let (newH, newA) = markFrom h a in (newH, newA : newStack)) (heap, [])
markFromDump :: TiHeap -> TiDump -> (TiHeap, TiDump)
markFromDump heap dump = (heap, dump)
markFromGlobals :: TiHeap -> TiGlobals -> (TiHeap, TiGlobals)
markFromGlobals heap = foldr (\(name, a) (h, newGlobals) -> let (newH, newA) = markFrom h a in (newH, (name, newA) : newGlobals)) (heap, [])
