module MarkScanGC where
import Heap (Addr, heapUpdate, heapLookup, addrsOf, heapFree)
import Compiler (Node (IndirectNode, Marked, Application, Data), TiDump, TiGlobals, TiHeap, TiStack, TiState)
import Debug.Trace (trace)

findStackRoots :: TiStack -> [Addr]
findStackRoots stack = stack

findDumpRoots :: TiDump -> [Addr]
findDumpRoots dump = []

findGlobalRoots :: TiGlobals -> [Addr]
findGlobalRoots = foldl (\b (_, addr) -> addr : b) []

markFrom :: TiHeap -> Addr -> TiHeap
markFrom heap addr = do
  let node = heapLookup heap addr
  case node of
    Marked _ -> heap
    _        -> markInner node (heapUpdate heap addr (Marked node))
  where
    markInner (IndirectNode a) h = markFrom h a
    markInner (Application a0 a1) h = markAddrs h [a0, a1]
    markInner (Data _ addrs) h = markAddrs h addrs
    markInner _ h = h
    markAddrs = foldl markFrom

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
  let rs = findRoots state
  let newHeap = foldl markFrom heap rs
  (output, stack, dump, scan newHeap, global, stats)

