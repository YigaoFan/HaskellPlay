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
isApplication heap addr =
  case heapLookup heap addr of
    Application _ _ -> True
    _               -> False
isPrim heap addr =
  case heapLookup heap addr of
    Prim _ _ -> True
    _        -> False

isFinal :: (Addr, Maybe Addr, TiHeap) -> Node -> Maybe Node -> Bool
isFinal (_, Nothing, heap) (Marked Done _) _ = True
isFinal _ _ _ = False
-- So good to add these two args to let them can be pattern match

-- 需要为这里专门搞个 Application 类型来兼容不同的 a1 a2 类型
mark :: (Addr, Maybe Addr, TiHeap) -> Node -> Maybe Node -> (Addr, Maybe Addr, TiHeap)
mark (_, backward, heap) (IndirectNode a) _ = (a, backward, heap)
mark (f, backward, heap) node@(Application a1 a2) _ =
  (a1, Just f, heapUpdate heap f (Marked (Visits 1 [] backward) node))
mark (f, backward@(Just b), heap) (Marked Done _) (Just (Marked (Visits 1 [] backward') node@(Application _ a2)))
  = (a2, backward, heapUpdate heap b (Marked (Visits 2 [f] backward') node))
mark (f, Just b, heap) (Marked Done _) (Just (Marked (Visits 2 [a1] backward') node@(Application {}))) -- 检查 backward' 的正确性
  = (b, backward', heapUpdate heap b (Marked Done (Application a1 f))) -- think about [Addr] here
-- 不确定这个b'是不是有效地址
mark (f, backward, heap) node _ =            (f, backward, heapUpdate heap f (Marked Done node))

-- TODO change
markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
markFrom heap addr = do
  let (f, _, h) = iter (addr, Nothing, heap) (heapLookup heap addr) Nothing in
    (h, f) -- 之前那个返回的地址怎么处理
  where
    iter :: (Addr, Maybe Addr, TiHeap) -> Node -> Maybe Node -> (Addr, Maybe Addr, TiHeap)
    iter state@(f, b, h) fwdNode bwdNode = 
      if isFinal state fwdNode bwdNode
        then state -- 之前那个返回的地址怎么处理
        else let s@(f', b', h') = mark state fwdNode bwdNode in
          iter s (heapLookup h' f') 
          (case b' of
            Just a -> Just (heapLookup h' a)
            _ -> Nothing)

    -- then (h, addr) --addr here not correct
    -- else markFrom h addr
  -- let newS@(_, _, h) = uncurry mark s
  -- let node = heapLookup heap addr
  -- case node of
  --   Marked _          -> (heap, addr)
  --   IndirectNode a    -> markFrom heap a
  --   _                 -> (markInner node (heapUpdate heap addr (Marked node)), addr)
  -- where
  --   markInner (Application a0 a1) h =
  --     let (newH, newAddrs) = markAddrs h [a0, a1] in
  --       heapUpdate newH addr (Marked (Application (head newAddrs) (newAddrs!!1)))
  --   markInner (Data t addrs) h =
  --     let (newH, newAddrs) = markAddrs h addrs
  --      in heapUpdate newH addr (Marked (Data t newAddrs))
  --   markInner _ h = h
  --   markAddrs h = foldr
  --     (\a (prevH, addrs) -> let (newH, newA) = markFrom prevH a in (newH, newA : addrs))
  --     (h, [])

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
