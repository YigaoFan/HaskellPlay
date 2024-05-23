module GMachine.Evaluator where
import GMachine.Util (Node(..), GmState (stats, code, globals, stack, heap), setStats, incStatSteps, setCode, Instruction (PushGlobal, PushInt, MakeApplication, Slide, Push, Unwind), setStack, setHeap, setGlobals)
import AST (Name)
import Heap (lookup, heapAlloc)
import Prelude hiding (lookup)

eval :: GmState -> [GmState]
eval state = state : remain
  where
    remain | gmFinal state = []
           | otherwise = eval nextState
    nextState = doAdmin (step state)
doAdmin :: GmState -> GmState
doAdmin s = setStats (incStatSteps (stats s)) s

gmFinal :: GmState -> Bool
gmFinal s =
  case code s of
    [] -> True
    _ -> False

step :: GmState -> GmState
step state = dispatch i (setCode is state)
  where (i : is) = code state

dispatch :: Instruction -> GmState -> GmState
dispatch (PushGlobal f) = pushGlobal f
dispatch (PushInt n) = pushInt n
dispatch MakeApplication = makeApplication
dispatch (Push n) = push n
dispatch (Slide n) = slide n
dispatch Unwind = unwindx

pushGlobal :: Name -> GmState -> GmState
pushGlobal f state =
  setStack (a : stack state) state
  where a = lookup (globals state) f (error ("Undeclared global " ++ f))

pushInt :: Int -> GmState -> GmState
pushInt n state =
  setHeap heap' (setStack (a : stack state) state)
  where (heap', a) = heapAlloc (heap state) (Num n)

makeApplication :: GmState -> GmState
makeApplication state =
  setHeap heap' (setStack (a : as') state)
  where
    (heap', a) = heapAlloc (heap state) (Application a1 a2)
    (a1 : a2 : as') = stack state
