module Evaluator where
import Compiler (TiState, isDataNode, applyToStats, incTiStatSteps, Node(..), TiStack, TiGlobals, TiHeap, Assoc, getTiStatSteps, Primitive (..), append, Stack (Stack), push, len)
import Heap (Addr, heapAlloc, heapLookup, lookup, heapUpdate)
import AST (CoreExpr, Name, SuperCombinator, nonRecursive, recursive, Alter)
import qualified AST (Expr (..))
import Prelude hiding (lookup, concat)
import PrettyPrint (display, concat, layn, Sequence (Newline, Str, Indent, Append), interleave, fillSpaceNum)
import Text.Printf (printf)

eval :: TiState -> [TiState]
eval state = state : remain -- Ex 2.9 由于 Haskell 里惰性求值的存在，所以这样写把 state 隔离到求值 remain 的影响外，不管怎么样，都会有个 state 可以读到
  where
    remain | tiFinal state = []
           | otherwise     =  eval nextState
    nextState = doAdmin (step state)

tiFinal :: TiState -> Bool

tiFinal (_, [addr], Stack [], heap, globals, stats) = isDataNode (heapLookup heap addr) -- TODO 这个要要求只能有一个吗？
tiFinal (_, [], Stack [], _, _, _) = True -- for Stop primitive
tiFinal (_, [], d, _, _, _) = error ("Empty stack! dump size " ++ show (len d))
tiFinal state = False
doAdmin :: TiState -> TiState
doAdmin = applyToStats incTiStatSteps
step :: TiState -> TiState
step state@(output, stack, dump, heap, globals, stats) =
  dispatch (heapLookup heap (head stack))
  where
    dispatch (String s) = stepString state s
    dispatch (Num n) = stepNum state n
    dispatch (Application a0 a1) = stepApplication state a0 a1
    dispatch (SuperCombinator name args body) = stepSuperCombinator state name args body
    dispatch (IndirectNode a) = (output, a : tail stack, dump, heap, globals, stats)
    dispatch (Prim name primitive) = stepPrim state primitive
    dispatch (Data tag coms) = stepData state tag coms
stepDataNode :: TiState -> TiState
stepDataNode (o, stack, Stack (oldSize : dump), heap, globals, stats) =
  if size < oldSize
    then error $ printf "stack maybe destroyed due to current stack size(%d) is smaller than old stack size(%d)" size oldSize
    else (o, drop (size - oldSize) stack, Stack dump, heap, globals, stats)
  where size = length stack
  
stepData :: TiState -> Int -> [Addr] -> TiState
stepData state tag coms = stepDataNode state
stepString :: TiState -> String -> TiState
stepString state s = stepDataNode state
stepNum :: TiState -> Int -> TiState
stepNum state _ = stepDataNode state

stepApplication :: TiState -> Addr -> Addr -> TiState
stepApplication (output, stack@(a : _), dump, heap, globals, stats) a0 a1 =
  let node = heapLookup heap a1 in
    case node of
      (IndirectNode addr) -> (output, stack, dump, heapUpdate heap a (Application a0 addr), globals, stats)
      _ -> (output, a0 : stack, dump, heap, globals, stats) -- put function into stack to make it eval function firstly

stepSuperCombinator :: TiState -> Name -> [Name] -> CoreExpr -> TiState
stepSuperCombinator (output, stack, dump, heap, globals, stats) name argNames body =
  if length argBinds == length argNames
    then (output, newStack, dump, newHeap, globals, stats)
    else error $ printf "pass too few arguments to %s: %d from stack len(%d)" name (length argBinds) (length stack)
  where
    argBinds = zip argNames (getArgs heap stack (length argNames)) -- args 在这里和下面都消费了栈上的项
    env = argBinds ++ globals
    newHeap = instantiateAndUpdate body (stack !! length argNames) heap env
    newStack = drop (length argNames) stack -- instantiate 到 heap 上，然后把函数的body再推到栈上运行
stepPrim :: TiState -> Primitive -> TiState
stepPrim state Neg = primNeg state
stepPrim state Add = primArith state (+)
stepPrim state Sub = primArith state (-)
stepPrim state Mul = primArith state (*)
stepPrim state Div = primArith state div
stepPrim state (Construct tag arity) = primConstr state tag arity
stepPrim state If = primIf state
stepPrim state Greater = primDyadic state (compareOperator (>))
stepPrim state GreaterEq = primDyadic state (compareOperator (>=))
stepPrim state Less = primDyadic state (compareOperator (<))
stepPrim state LessEq = primDyadic state (compareOperator (<=))
stepPrim state Eq = primDyadic state (compareOperator (==))
stepPrim state NotEq = primDyadic state (compareOperator (/=))
stepPrim state CasePair = primCasePair state
stepPrim state CaseList = primCaseList state
stepPrim state Abort = primAbort state
stepPrim state Print = primPrint state
stepPrim state Stop = primStop state

compareOperator :: (Int -> Int -> Bool) -> Node -> Node -> Node
compareOperator op (Num n0) (Num n1) = if op n0 n1 then Data trueTag [] else Data falseTag []

primConstr :: TiState -> Int -> Int -> TiState
primConstr state@(output, stack, dump, heap, globals, stats) tag arity =
  if length argAddrs == arity
    then (output, newStack, dump, heapUpdate heap (head newStack) (Data tag argAddrs), globals, stats)
    else error "argAddrs is not match with arity"
  where
    argAddrs = getArgs heap stack arity
    newStack = drop (length argAddrs) stack
primIf :: TiState -> TiState
trueTag :: Int
trueTag = 2
falseTag :: Int
falseTag = 1
-- trueTag = 2, falseTag = 1
primIf state@(output, stack@(a : a1 : a2 : a3 : as), dump, heap, globals, stats) =
  if isDataNode condObj
    then -- why use IndirectNode here
      (output, a3 : as, dump, heapUpdate heap a3 (IndirectNode (operandAddrOf 
        (case condObj of
          (Data 2 _) -> a2
          (Data 1 _) -> a3)
        heap)), globals, stats)
    else (output, cond : stack, push dump (length stack), heap, globals, stats)
  where
    cond = operandAddrOf a1 heap
    condObj = heapLookup heap cond -- 原来这里写 condObj@(Data tag _) 就会解包，即使上面只用了 condObj，毕竟你这里假设了他是个 Data obj 了

-- 不在 dump 里存 stack 后，这里的 stack@[a, a1] 应该匹配不上了吧
-- 所有类似上面这种的 stack 匹配的都会失败
-- 凡是涉及栈切换的都要跟着改 dump，一个栈的终点或标志性就是一个数据节点
primNeg :: TiState -> TiState
primNeg (output, stack@(a : a1 : as), dump, heap, globals, stats) = -- stack only contains two items in real world? Yes
  if isDataNode (heapLookup heap b)
    then (output, a1 : as, dump, heapUpdate heap a1 (Num (-n)), globals, stats)
    else (output, b : a1 : as, push dump (length stack - 1), heap, globals, stats)
  where
    b = operandAddrOf a1 heap
    (Num n) = heapLookup heap b
-- defend programming
primNeg (_, stack, _, _, _, _) = error ("wrong arguments for primNeg " ++ show (length stack))

operandAddrOf :: Addr -> TiHeap -> Addr
operandAddrOf appAddr heap =
  let (Application _ addr) = heapLookup heap appAddr
   in addr

-- 如果不想在这个函数一下子完成所有计算，而是将状态变化显化出来、转移出去，就得想办法让它转移出来再回来
primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state@(output, stack@(_ : a1 : a2 : as), dump, heap, globals, stats) op =
  if isOperandDataNode a1
    then if isOperandDataNode a2
      then (output, [a2], dump, heapUpdate heap a2 (Num (op (operandDataOf a1) (operandDataOf a2))), globals, stats)
      else (output, operandAddrOf a2 heap : a2 : as, push dump (length stack - 2), heap, globals, stats)
    else (output, operandAddrOf a1 heap : a1 : a2 : as, push dump (length stack - 1), heap, globals, stats)
  where
    isOperandDataNode appAddr =
      let addr = operandAddrOf appAddr heap
       in isDataNode (heapLookup heap addr)
    operandDataOf appAddr =
      let addr = operandAddrOf appAddr heap
          (Num n) = heapLookup heap addr
       in n

primDyadic :: TiState -> (Node -> Node -> Node) -> TiState
primDyadic state@(output, stack@(_ : a1 : a2 : as), dump, heap, globals, stats) op =
  if isOperandDataNode a1
    then if isOperandDataNode a2
      then (output, [a2], dump, heapUpdate heap a2 (op (operandNodeOf a1) (operandNodeOf a2)), globals, stats)
      else (output, operandAddrOf a2 heap : a2 : as, push dump (length stack - 2), heap, globals, stats)
    else (output, operandAddrOf a1 heap : a1 : a2 :as, push dump (length stack - 1), heap, globals, stats)
  where
    isOperandDataNode appAddr =
      let addr = operandAddrOf appAddr heap
       in isDataNode (heapLookup heap addr)
    operandNodeOf appAddr =
      let addr = operandAddrOf appAddr heap
        in heapLookup heap addr

-- 下面这个应该与 primIf 类似
primCasePair :: TiState -> TiState
primCasePair state@(output, stack@(a : a1 : a2 : as), dump, heap, globals, stats) =
  if isDataNode pairNode
    then let
      (Data t coms@[com, com1]) = pairNode -- make sure it's pair
    in do
      let (heap1, appAddr) = heapAlloc heap (Application (operandAddrOf a2 heap) com)
      (output, [a2], dump, heapUpdate heap1 a2 (Application appAddr com1), globals, stats)
    else (output, operandAddrOf a1 heap : a1 : a2 : as, push dump (length stack - 1), heap, globals, stats)
    -- 这里 eval 完了可能 operandAddrOf a1 heap 变成了 Indirect，所以 dump 那里要把 a 去掉，让 a1 (Application )的参数触发参数更新，
    -- 把 Indirect 去掉，不然这个 pairNode 一直都是非 DataNode，无限循环在这里的 else 分支里
  where
    pairNode = heapLookup heap (operandAddrOf a1 heap)
-- 与 primCasePair 类似
primCaseList :: TiState -> TiState
primCaseList state@(output, stack@(a : a1 : a2 : a3 : as), dump, heap, globals, stats) =
  if isDataNode listNode
    then case listNode of
        -- 注意下面是栈变为从 a3 开始、也更新 heap，相当于改变执行流了
        -- 注意你要用地址的时候，用的是 Application 本身，还是 Application 的第二个参数
        (Data 1 []) -> (output, [a3], dump, heapUpdate heap a3 (IndirectNode (operandAddrOf a2 heap)), globals, stats)
        (Data 2 [com, com1]) -> let (heap1, appAddr) = heapAlloc heap (Application (operandAddrOf a3 heap) com)
          in (output, [a3], dump, heapUpdate heap1 a3 (Application appAddr com1), globals, stats)
        _ -> error "other not supported type list node"
    else (output, operandAddrOf a1 heap : a1 : a2 : a3 : as, push dump (length stack - 1), heap, globals, stats)
  where
    listNode = heapLookup heap (operandAddrOf a1 heap)
primCaseList state@(output, stack, dump, heap, globals, stats) =
  error ("arg match issue " ++ show (length stack) ++ display (showStack heap stack) ++ display (showHeap heap))

primAbort :: TiState -> TiState
primAbort state@(output, stack@(_ : a1 : as), dump, heap, globals, stats) =
  error ("program aborted: " ++ msg)
  where
    (String msg) = heapLookup heap (operandAddrOf a1 heap)

primPrint :: TiState -> TiState -- 为什么也要求 dump 为空？我猜是因为要走 continuation，不走 dump 的后续了，这样其实破坏了原来程序的后续流程
primPrint (output, stack@(a : a1 : a2 : as), dump@(Stack []), heap, globals, stats) =
  if isDataNode node
    then let (Num n) = node
    in (append output n, [continuation], Stack [], heap, globals, stats)
    else (output, firstArgAddr : a1 : a2 : as, push dump (length stack - 1), heap, globals, stats)
  where
    continuation = operandAddrOf a2 heap
    firstArgAddr = operandAddrOf a1 heap
    node = heapLookup heap firstArgAddr

primStop :: TiState -> TiState
primStop (output, stack, dump, heap, globals, stats) = (output, [], dump, heap, globals, stats)

-- why getArgs do like this? 思考整个 evaluator 的过程。
-- 懂了，由于压栈和都是单参数函数调用，所以栈上当前函数调用的参数，都在下一个栈项 Application 的第二个参数，即下面的 arg 上
getArgs :: TiHeap -> TiStack -> Int -> [Addr]
getArgs heap (sc : stack) argsCount = map getArg (take argsCount stack)
  where getArg addr = arg where (Application _ arg) = heapLookup heap addr

instantiate :: CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiate (AST.Num n) heap env = heapAlloc heap (Num n)
instantiate (AST.String s) heap env = heapAlloc heap (String s)
instantiate (AST.Application e0 e1) heap env = heapAlloc heap2 (Application a0 a1)
  where
    (heap1, a0) = instantiate e0 heap env
    (heap2, a1) = instantiate e1 heap1 env
instantiate (AST.Var v) heap env = (heap, lookup env v (error ("undefined name " ++ show v)))
instantiate (AST.Constructor tag arity) heap env =
  heapAlloc heap (Prim "Pack" (Construct tag arity))
instantiate (AST.Let False defs body) heap env =
  instantiate body newHeap newEnv
  where
    (newHeap, newEnv) =
      foldl
      (\(h, e) (n, exp) ->
        let
          (h', addr) = instantiate exp h e
          in (h', (n, addr) : e))
      (heap, env)
      defs
instantiate (AST.Let True defs body) heap env =
  instantiate body newHeap newEnv
  where
    (newHeap, addrs) =
      foldl
        (\(h, ads) (_, exp) ->
            let (h', addr) = instantiate exp h newEnv -- does this work?
             in (h', addr : ads))
        (heap, [])
        defs
    newEnv = zip (map fst defs) addrs ++ env
instantiate (AST.Case e alts) heap env =
  error "Can't instantiate case expr"
instantiate (AST.Lambda paras body) heap env =
  error "Can't instantiate lambda expr"

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> Assoc Name Addr -> TiHeap
instantiateAndUpdate (AST.Application e0 e1) updateAddr heap env =
  heapUpdate heap2 updateAddr (Application a0 a1)
  where
    (heap1, a0) = instantiate e0 heap env -- why call normal instantiate，因为这个内部的表达式外部不可能共享？内部共享呢？
    (heap2, a1) = instantiate e1 heap1 env
instantiateAndUpdate (AST.String s) updateAddr heap env =
  heapUpdate heap updateAddr (String s)
instantiateAndUpdate (AST.Num n) updateAddr heap env =
  heapUpdate heap updateAddr (Num n)
instantiateAndUpdate (AST.Var v) updateAddr heap env = -- use Indirect to prevent dumplication eval of the value of var
  heapUpdate heap updateAddr (IndirectNode (lookup env v (error ("undefined name " ++ show v))))
instantiateAndUpdate (AST.Let False defs body) updateAddr heap env =
  heapUpdate heapAfterBody updateAddr (heapLookup heapAfterBody addr)
    where
      (newHeap, addrs) =
        foldl
          (\(h, ads) (_, exp) ->
              let (h', addr) = instantiate exp h newEnv -- does this work?
              in (h', addr : ads))
          (heap, [])
          defs
      newEnv = zip (map fst defs) addrs ++ env
      (heapAfterBody, addr) = instantiate body newHeap newEnv
instantiateAndUpdate (AST.Let True defs body) updateAddr heap env =
  heapUpdate newHeap updateAddr (heapLookup newHeap addr) -- 我还没想出 Let 这里除了 update，和 instantiate 要有什么不同
  where
    (heap', env') =
      foldl
        (\(h, e) (n, exp) -> -- 我写的这个真的可以吗？那个卡死是因为什么？这里我是写了个没用的死递归吧，我这里没结束点
         -- 因为这里虽然是递归定义的，但是是希望能运行结束。非当前变量的递归是支持的，当前变量递归不行。
            let (h', addr) = instantiate exp h env' --recursive TODO test when support lazy evaluation
             in (h', (n, addr) : e))
        (heap, env)
        defs
    (newHeap, addr) = instantiate body heap' env'
instantiateAndUpdate (AST.Constructor tag arity) updateAddr heap env =
  heapUpdate heap updateAddr (Prim "Pack" (Construct tag arity))
instantiateAndUpdate _ _ _ _ = error "Can't instantiateAndUpdate now"
showResults :: [TiState] -> String
showResults states =
  display (concat [
    showOutput (last states),
    layn (map showState states),
    showStats (last states)
  ])

showOutput :: TiState -> Sequence
showOutput state@(output, _, _, _, _, _) =
  concat [
    Str "output [",
    interleave (Str ", ") (map (Str . show) (reverse output)),
    Str "]",
    Newline
  ]
showStats :: TiState -> Sequence
showStats (output, stack, dump, heap, globals, stats) =
  concat [
    Newline, showHeap heap,
    Newline, Str "Total number of steps = ",
    Str (show (getTiStatSteps stats))
  ]

showState :: TiState -> Sequence
showState (output, stack, dump, heap, globals, stats) = showStack heap stack

showStack :: TiHeap -> TiStack -> Sequence
showStack heap stack =
  concat [
    Str "Stack [",
    Indent (interleave (concat [Str "," , Newline]) (map showStackItem stack)),
    Str " ]",
    Newline
  ]
  where showStackItem addr = concat [fillSpaceShowAddr addr, Str ": ", showStackNode heap (heapLookup heap addr)]

fillSpaceShowAddr :: Addr -> Sequence
fillSpaceShowAddr = fillSpaceNum 2

showStackNode :: TiHeap -> Node -> Sequence
showStackNode heap (Application funcAddr argAddr) =
  concat [
    Str "Application ", fillSpaceShowAddr funcAddr,
    Str " ", fillSpaceShowAddr argAddr,
    Str " (", showNode (heapLookup heap argAddr), Str ")"
  ]
showStackNode heap node@(IndirectNode addr) =
  concat [
    showNode node,
    Str " (", showNode (heapLookup heap addr), Str ")"
  ]
showStackNode heap node = showNode node

showNode :: Node -> Sequence
showNode (Application a0 a1) =
  concat [
    Str "Application ", showAddr a0,
    Str " ",            showAddr a1
  ]
showNode (SuperCombinator name args body) =
  Str ("Supercombinator " ++ name)
showNode (Num n) = Str "Num " `Append` Str (show n)
showNode (IndirectNode a) =
  concat [
    Str "IndirectTo ", Str (show a)
  ]
showNode (Prim name prim) =
  concat [
    Str "Prim ", Str name
  ]
showNode (Data tag coms) =
  concat [
    Str "Data ", Str (show tag), Str " [",
    interleave (Str ", ") (map showAddr coms),
    Str "]"
  ]
showNode (String s) =
  Str "Str " `Append` Str s
showAddr :: Addr -> Sequence
showAddr = Str . show

showHeap :: TiHeap -> Sequence
showHeap (size, free, addrObjs) =
  concat [
    Str "count: ", Str (show size), Newline,
    Str "{", Newline,
    interleave Newline (map (\(addr, obj) -> concat [showAddr addr, Str " : ", showNode obj]) addrObjs), Newline,
    Str "}", Newline
  ]
