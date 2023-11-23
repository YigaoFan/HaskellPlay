module Evaluator where
import Compiler (TiState, isDataNode, applyToStats, incTiStatSteps, Node(..), TiStack, TiGlobals, TiHeap, Assoc, getTiStatSteps, Primitive (..))
import Heap (Addr, heapAlloc, heapLookup, lookup, heapUpdate)
import AST (CoreExpr, Name, SuperCombinator, nonRecursive, recursive)
import qualified AST (Expr (..))
import Prelude hiding (lookup, concat)
import PrettyPrint (display, concat, layn, Sequence (Newline, Str, Indent, Append), interleave, fillSpaceNum)

eval :: TiState -> [TiState]
eval state = state : remain -- Ex 2.9 由于 Haskell 里惰性求值的存在，所以这样写把 state 隔离到求值 remain 的影响外，不管怎么样，都会有个 state 可以读到
  where
    remain | tiFinal state = []
           | otherwise     =  eval nextState
    nextState = doAdmin (step state)

tiFinal :: TiState -> Bool
tiFinal ([addr], [], heap, globals, stats) = isDataNode (heapLookup heap addr)
tiFinal ([], d, _, _, _) = error ("Empty stack! dump size " ++ show (length d))
tiFinal state = False
doAdmin :: TiState -> TiState
doAdmin = applyToStats incTiStatSteps
step :: TiState -> TiState
step state@(stack, dump, heap, globals, stats) =
  dispatch (heapLookup heap (head stack))
  where
    dispatch (Num n) = stepNum state n
    dispatch (Application a0 a1) = stepApplication state a0 a1
    dispatch (SuperCombinator name args body) = stepSuperCombinator state name args body
    dispatch (IndirectNode a) = (a : tail stack, dump, heap, globals, stats)
    dispatch (Prim name primitive) = stepPrim state primitive
stepNum :: TiState -> Int -> TiState
stepNum ([_], s : dump, heap, globals, stats) _ = (s, dump, heap, globals, stats)
stepNum _ _ = error "Number applied as a function!"
stepApplication :: TiState -> Addr -> Addr -> TiState
stepApplication (stack@(a : _), dump, heap, globals, stats) a0 a1 =
  let node = heapLookup heap a1 in
    case node of
      (IndirectNode addr) -> (stack, dump, heapUpdate heap a (Application a0 addr), globals, stats)
      _ -> (a0 : stack, dump, heap, globals, stats) -- put function into stack to make it eval function firstly

stepSuperCombinator :: TiState -> Name -> [Name] -> CoreExpr -> TiState
stepSuperCombinator (stack, dump, heap, globals, stats) name argNames body =
  if length argBinds == length argNames
    then (newStack, dump, newHeap, globals, stats)
    else error ("pass too few arguments to " ++ name)
  where
    argBinds = zip argNames (getArgs heap stack) -- args 在这里和下面都消费了栈上的项
    env = argBinds ++ globals
    newHeap = instantiateAndUpdate body (stack !! length argNames) heap env
    newStack = drop (length argNames) stack -- instantiate 到 heap 上，然后把函数的body再推到栈上运行
stepPrim :: TiState -> Primitive -> TiState
stepPrim state Neg = primNeg state
stepPrim state Add = primArith state (+)
stepPrim state Sub = primArith state (-)
stepPrim state Mul = primArith state (*)
stepPrim state Div = primArith state div

primNeg :: TiState -> TiState
primNeg (stack@[a, a1], dump, heap, globals, stats) = -- stack only contains two items in real world? Yes
  if isDataNode (heapLookup heap b)
    then ([a1], dump, heapUpdate heap a1 (Num (-n)), globals, stats)
    else ([b], [a1] : dump, heap, globals, stats)
  where
    (Application _ b) = heapLookup heap a1
    (Num n) = heapLookup heap b
primNeg (stack, _, _, _, _) = error ("wrong arguments for primNeg " ++ show (length stack))

operandAddrOf :: Addr -> TiHeap -> Addr
operandAddrOf appAddr heap =
  let (Application _ addr) = heapLookup heap appAddr
   in addr
evalArithNum :: Addr -> TiState -> TiState
evalArithNum appAddr state@(_, dump, heap, globals, stats) =
  ([operandAddrOf appAddr heap], dump, heap, globals, stats)

-- 如果不想在这个函数一下子完成所有计算，而是将状态变化显化出来、转移出去，就得想办法让它转移出来再回来
primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith state@(stack@[_, a1, a2], dump, heap, globals, stats) op =
  if isOperandDataNode a1
    then if isOperandDataNode a2
      then ([a2], dump, heapUpdate heap a2 (Num (op (operandDataOf a1) (operandDataOf a2))), globals, stats)
      else evalArithNum a2 (stack, [a2] : dump, heap, globals, stats)
    else evalArithNum a1 (stack, [a1, a2] : dump, heap, globals, stats)
  where
    isOperandDataNode appAddr =
      let addr = operandAddrOf appAddr heap
       in isDataNode (heapLookup heap addr)
    operandDataOf appAddr =
      let addr = operandAddrOf appAddr heap
          (Num n) = heapLookup heap addr
       in n


-- why getArgs do like this? 思考整个 evaluator 的过程。
-- 懂了，由于压栈和都是单参数函数调用，所以栈上当前函数调用的参数，都在下一个栈项 Application 的第二个参数，即下面的 arg 上
getArgs :: TiHeap -> TiStack -> [Addr]
getArgs heap (sc : stack) = map getArg stack
  where getArg addr = arg where (Application _ arg) = heapLookup heap addr

instantiate :: CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiate (AST.Num n) heap env = heapAlloc heap (Num n)
instantiate (AST.Application e0 e1) heap env = heapAlloc heap2 (Application a0 a1)
  where
    (heap1, a0) = instantiate e0 heap env
    (heap2, a1) = instantiate e1 heap1 env
instantiate (AST.Var v) heap env = (heap, lookup env v (error ("undefined name " ++ show v)))
instantiate (AST.Constructor tag arity) heap env =
  error "Can't instantiate constructor expr"
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

instantiateAndUpdate :: CoreExpr -> Addr -> TiHeap -> Assoc Name Addr -> TiHeap
instantiateAndUpdate (AST.Application e0 e1) updateAddr heap env =
  heapUpdate heap2 updateAddr (Application a0 a1)
  where
    (heap1, a0) = instantiate e0 heap env -- why call normal instantiate，因为这个内部的表达式外部不可能共享？内部共享呢？
    (heap2, a1) = instantiate e1 heap1 env
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

instantiateAndUpdate _ _ _ _ = error "Can't instantiateAndUpdate now"
showResults states =
  display (concat [layn (map showState states), showStats (last states)])

showStats :: TiState -> Sequence
showStats (stack, dump, heap, globals, stats) =
  concat [
    Newline, showHeap heap,
    Newline, Str "Total number of steps = ",
    Str (show (getTiStatSteps stats))
  ]

showState :: TiState -> Sequence
showState (stack, dump, heap, globals, stats) = showStack heap stack

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
showAddr :: Addr -> Sequence
showAddr = Str . show

showHeap :: TiHeap -> Sequence
showHeap (size, free, addrObjs) =
  concat [
    Str "count: ", Str (show size), Newline,
    Str "{", Newline,
    interleave Newline (map (\(addr, obj) -> concat [Str (show addr), Str " : ", showNode obj]) addrObjs), Newline,
    Str "}", Newline
  ]
