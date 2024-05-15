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
evacuateStack :: TiHeap -> TiHeap -> TiStack -> (TiHeap, TiHeap, TiStack)
evacuateStack fromHeap toHeap =
  foldr (\a (fromHeap', toHeap', s) ->
    case heapLookup fromHeap' a of --recursive allocate?
      Forward addr -> (fromHeap', toHeap', addr : s)
      node -> let (newH, newA) = heapAlloc toHeap' node in 
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
