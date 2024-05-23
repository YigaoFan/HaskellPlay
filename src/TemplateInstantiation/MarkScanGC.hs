module TemplateInstantiation.MarkScanGC where
import Heap (Addr, heapUpdate, heapLookup, addrsOf, heapFree)
import TemplateInstantiation.Compiler (MarkState (Done, Visits), Node (Application, Data, IndirectNode, Marked, Num, Prim, String, SuperCombinator), TiDump, TiGlobals, TiHeap, TiStack, TiState)
import Debug.Trace (trace)
import Data.Maybe (Maybe)
import GHC.Stack (HasCallStack)

findStackRoots :: TiStack -> [Addr]
findStackRoots stack = stack

findDumpRoots :: TiDump -> [Addr]
findDumpRoots dump = []

findGlobalRoots :: TiGlobals -> [Addr]
findGlobalRoots = foldr (\(_, addr) b -> addr : b) []

isFinal :: (Addr, Maybe Addr, TiHeap) -> Node -> Maybe Node -> Bool
isFinal (_, Nothing, heap) (Marked Done _) Nothing = True
isFinal _ _ _ = False
-- So good to add these two args to let them can be pattern match
-- 需要为这里专门搞个 Application 类型来兼容不同的 a1 a2 类型，后来思考下程序的语义给 Visits 里加了两项
-- 这里面涉及的地址都有可能要更新啊，不能直接用原来的 node
mark :: (Addr, Maybe Addr, TiHeap) -> Node -> Maybe Node -> (Addr, Maybe Addr, TiHeap)
mark (_, backward, heap) (IndirectNode a) _ = (a, backward, heap)

mark (f, backward, heap) node@(Application a1 a2) _ =
  (a1, Just f, heapUpdate heap f (Marked (Visits 1 [] backward) node))
mark (f, backward@(Just b), heap) (Marked Done _) (Just (Marked (Visits 1 [] backward') node@(Application _ a2))) =
  (a2, backward, heapUpdate heap b (Marked (Visits 2 [f] backward') node))
mark (f, Just b, heap) (Marked Done _) (Just (Marked (Visits 2 [a1] backward') node@(Application {}))) =
  (b, backward', heapUpdate heap b (Marked Done (Application a1 f)))

mark (f, backward, heap) node@(Data _ []) _ =
  (f, backward, heapUpdate heap f (Marked Done node))
mark (f, backward, heap) node@(Data _ (a1 : addrs)) _ =
  (a1, Just f, heapUpdate heap f (Marked (Visits 1 [] backward) node))
mark (f, backward@(Just b), heap) (Marked Done _) (Just (Marked (Visits n visits backward') node@(Data t coms))) =
  let newVisits = f : visits in
    if n == length coms
      then (b, backward', heapUpdate heap b (Marked Done (Data t (reverse newVisits))))
      else (coms!!n, backward, heapUpdate heap b (Marked (Visits (n+1) newVisits backward') node))

mark (f, backward, heap) node@(SuperCombinator {}) _ = (f, backward, heapUpdate heap f (Marked Done node))
mark (f, backward, heap) node@(Num {}) _ = (f, backward, heapUpdate heap f (Marked Done node))
mark (f, backward, heap) node@(String {}) _ = (f, backward, heapUpdate heap f (Marked Done node))
mark (f, backward, heap) node@(Prim {}) _ = (f, backward, heapUpdate heap f (Marked Done node))
mark _ fwdNode bwdNode = error ("no match function mark" ++ show fwdNode ++ show bwdNode)

markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
markFrom heap addr = do
  let (f, _, h) = iter (addr, Nothing, heap) in
    (h, f)
  where
    iter state@(f, b, h) =
      let
        fwdNode = heapLookup h f
        bwdNode =
          (case b of
            Just a -> Just (heapLookup h a)
            _ -> Nothing)
        in
        if isFinal state fwdNode bwdNode
          then state -- 之前那个返回的地址怎么处理：用 final state 下的 f，观察下来此时 f 就对应最初的那个节点的 Marked Done
          else iter $ mark state fwdNode bwdNode

scan :: TiHeap -> TiHeap
scan heap = do
  let addrs = addrsOf heap
  foldl (\h a ->
    case heapLookup h a of
      Marked Done node -> heapUpdate h a node
      _ -> heapFree h a
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
