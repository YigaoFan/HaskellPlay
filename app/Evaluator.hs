module Evaluator where
import Compiler (TiState, isDataNode, applyToStats, incTiStatSteps, Node(..), TiStack, TiGlobals, TiHeap, Assoc, getTiStatSteps)
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
tiFinal ([addr], dump, heap, globals, stats)
  = isDataNode (heapLookup heap addr)
tiFinal ([], _, _, _, _) = error "Empty stack!"
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
stepNum :: TiState -> Int -> TiState
stepNum _ _ = error "Number applied as a function!"
stepApplication :: TiState -> Addr -> Addr -> TiState
stepApplication (stack, dump, heap, globals, stats) a0 a1 =
  (a0 : stack, dump, heap, globals, stats) -- put function into stack to make it eval function firstly
stepSuperCombinator :: TiState -> Name -> [Name] -> CoreExpr -> TiState
stepSuperCombinator (stack, dump, heap, globals, stats) name argNames body =
  if length argBinds == length argNames
    then (newStack, dump, heapUpdate newHeap (stack!!length argNames) (IndirectNode resultAddr), globals, stats)
    else error ("pass too few arguments to " ++ name)
  where
    argBinds = zip argNames (getArgs heap stack) -- args 在这里和下面都消费了栈上的项
    env = argBinds ++ globals
    (newHeap, resultAddr) = instantiate body heap env
    newStack = resultAddr : drop (length argNames + 1) stack -- instantiate 到 heap 上，然后把函数的body再推到栈上运行

-- TODO:
-- 修好 indent
-- 理解 indirect

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
    (newHeap, newEnv) =
      foldl
        (\(h, e) (n, exp) ->
            let (h', addr) = instantiate exp h newEnv -- does this work?
             in (h', (n, addr) : e)
        )
        (heap, env)
        defs
instantiate (AST.Case e alts) heap env =
  error "Can't instantiate case expr"

showResults states =
  display (concat [layn (map showState states), showStats (last states)])

showStats :: TiState -> Sequence
showStats (stack, dump, heap, globals, stats) =
  concat [
    Newline, Newline, Str "Total number of steps = ",
    Str (show (getTiStatSteps stats))
  ]

showState :: TiState -> Sequence
showState (stack, dump, heap, globals, stats) = showStack heap stack

showStack :: TiHeap -> TiStack -> Sequence
showStack heap stack =
  concat [
    Str "Stack [",
    Indent (interleave (concat [Str "," , Newline]) (map showStackItem stack)),
    Str " ]"
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
showAddr :: Addr -> Sequence
showAddr = Str . show
