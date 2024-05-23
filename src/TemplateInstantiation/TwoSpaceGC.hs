module TemplateInstantiation.TwoSpaceGC where
import TemplateInstantiation.Compiler (Node (Application, Data, Forward, IndirectNode, Num, Prim, String, SuperCombinator), TiDump, TiGlobals, TiHeap, TiStack, TiState)
import Heap (heapAlloc, heapLookup, addrsOf, Addr, heapUpdate, initHeap)

findNodeAddrsInStack :: TiStack -> TiHeap -> [Addr]
findNodeAddrsInStack stack heap
  = foldr
      (\a
         -> (++)
              (case heapLookup heap a of
                 Application a1 a2 -> [a, a1, a2]
                 IndirectNode a1 -> [a1]
                 Data _ addrs -> a : addrs
                 _ -> [a]))
      [] stack
findNodeAddrsInGlobals :: TiGlobals -> TiHeap -> [Addr]
findNodeAddrsInGlobals [] heap = []
findNodeAddrsInGlobals ((_ , a) : xs) heap = a : findNodeAddrsInGlobals xs heap
-- 这个函数式我怎么修改原来的 heap 呢？还有是不是要递归 copy 下去？
evacuate :: TiHeap -> TiHeap -> Addr -> (TiHeap, TiHeap, Addr)
evacuate fromHeap toHeap addr =
  let node = heapLookup fromHeap addr in
  case node of
    Forward a -> (fromHeap, toHeap, a)
    Application a1 a2 -> do
      let (from', to', addr') = copyThenMark addr node fromHeap toHeap
      let (from'', to'') = foldr (\a (f, t) -> let (newF, newT, _) = evacuate f t a in (newF, newT)) (from', to') [a1, a2]
      (from'', to'', addr')
    IndirectNode a -> let (f', t', a') = evacuate fromHeap toHeap a in (heapUpdate f' addr (Forward a'), t', a')
    Data _ addrs -> do
      let (from', to', addr') = copyThenMark addr node fromHeap toHeap
      let (from'', to'') = foldr (\a (f, t) -> let (newF, newT, _) = evacuate f t a in (newF, newT)) (from', to') addrs
      (from'', to'', addr')
    _ -> copyThenMark addr node fromHeap toHeap
  where
    copyThenMark a node from to = do
      let (newH, newA) = heapAlloc to node
      (heapUpdate from a (Forward newA), newH, newA)

evacuateStack :: TiHeap -> TiHeap -> TiStack -> (TiHeap, TiHeap, TiStack)
evacuateStack fromHeap toHeap =
  foldr (\a (from, to, s) -> let (f', t', a') = evacuate from to a in (f', t', a' : s))
    (fromHeap, toHeap, [])
evacuateDump :: TiHeap -> TiHeap -> TiDump -> (TiHeap, TiHeap, TiDump)
evacuateDump fromHeap toHeap dump = (fromHeap, toHeap, dump)
evacuateGlobals :: TiHeap -> TiHeap -> TiGlobals -> (TiHeap, TiHeap, TiGlobals)
evacuateGlobals fromHeap toHeap = foldr (\(n, a) (from, to, g) -> let (from', to', a') = evacuate from to a in (from', to', (n, a') : g)) (fromHeap, toHeap, [])

scavengeHeap :: TiHeap -> TiHeap -> TiHeap
scavengeHeap fromHeap toHeap =
  foldl
    (\h a ->
      heapUpdate h a
      (case heapLookup h a of
        Application a1 a2 -> Application (fwdAddrOf a1) (fwdAddrOf a2)
        n@SuperCombinator {} -> n
        n@Num {} -> n
        n@String {} -> n
        IndirectNode _ -> error "should not exist Indirect Node"
        n@Prim {} -> n
        Data t addrs -> Data t (map fwdAddrOf addrs))
    ) toHeap (addrsOf toHeap)
  where
    fwdAddrOf a = let (Forward a') = heapLookup fromHeap a in a'

gc :: TiState -> TiState
gc state@(output, stack, dump, heap, global, stats) = do
  let (from1, to1, stack1) = evacuateStack heap initHeap stack
  let (from2, to2, dump1) = evacuateDump from1 to1 dump
  let (from3, to3, global1) = evacuateGlobals from2 to2 global
  (output, stack1, dump1, scavengeHeap from3 to3, global1, stats)
