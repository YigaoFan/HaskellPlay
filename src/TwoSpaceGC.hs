module TwoSpaceGC where
import Compiler (TiHeap, TiStack, TiDump, TiGlobals, Node (Application, SuperCombinator, Num, String, IndirectNode, Prim, Data, Forward))
import Heap (heapAlloc, heapLookup, addrsOf, Addr, heapUpdate)

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
    Application a1 a2 -> copyThenMark addr node fromHeap toHeap
    IndirectNode a -> evacuate fromHeap toHeap a
    Data _ addrs -> do
      let (from', to', addr') = copyThenMark addr node fromHeap toHeap
      let (from'', to'') = foldr (\a (f, t) -> let (newF, newT, _) = evacuate f t a in (newF, newT)) (from', to') addrs
      (from'', to'', addr')
  where
    copyThenMark a node from to = do
      let (newH, newA) = heapAlloc to node
      (newH, heapUpdate from a (Forward newA), newA)
      

evacuateStack :: TiHeap -> TiHeap -> TiStack -> (TiHeap, TiHeap, TiStack)
evacuateStack fromHeap toHeap =
  foldr (\a (fromHeap', toHeap', s) ->
    case heapLookup fromHeap' a of
      Forward addr -> (fromHeap', toHeap', addr : s)
      node -> let (newH, newA) = heapAlloc toHeap' node in --recursive allocate?
        (newH, heapUpdate fromHeap' a (Forward newA), newA : s))
    (fromHeap, toHeap, [])
evacuateDump :: TiHeap -> TiHeap -> TiDump -> (TiHeap, TiDump)
evacuateDump fromHeap toHeap dump = (toHeap, dump)
evacuateGlobals :: TiHeap -> TiHeap -> TiGlobals -> (TiHeap, TiGlobals)
evacuateGlobals fromHeap toHeap = foldr (\(n, a) (h, g) -> let (newH, newA) = heapAlloc h (heapLookup fromHeap a) in (newH, (n, newA) : g)) (toHeap, [])

scavengeHeap :: TiHeap -> TiHeap -> TiHeap
scavengeHeap fromHeap toHeap =
  foldl
    (\b a ->
      case heapLookup b a of
        Application a1 a2 -> error ""
        SuperCombinator {} -> error ""
        Num {} -> error ""
        String {} -> error ""
        IndirectNode _ -> error "" -- 这个要怎么处理
        Prim {} -> error ""
        Data {} -> error ""
    ) toHeap (addrsOf toHeap)
