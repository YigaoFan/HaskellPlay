module MarkScanGC where
import Heap (Addr, heapUpdate, heapLookup, addrsOf, heapFree)
import Compiler (Node (IndirectNode, Marked, Application, Data), TiDump, TiGlobals, TiHeap, TiStack, TiState)
import Debug.Trace (trace)

findStackRoots :: TiStack -> [Addr]
findStackRoots stack = stack

findDumpRoots :: TiDump -> [Addr]
findDumpRoots dump = []

findGlobalRoots :: TiGlobals -> [Addr]
findGlobalRoots = foldr (\(_, addr) b -> addr : b) []

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
