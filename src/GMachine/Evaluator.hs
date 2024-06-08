module GMachine.Evaluator where
import GMachine.Util (Node(..), GmState (stats, code, globals, stack, heap), setStats, incStatSteps, setCode, Instruction (PushGlobal, PushInt, MakeApplication, Push, Unwind, Pop, Update, Alloc, Slide), setStack, setHeap, setGlobals, GmHeap, GmStack)
import AST (Name)
import Heap (lookup, heapAlloc, heapLookup, Addr, heapUpdate)
import Prelude hiding (lookup)
import Data.Foldable (find)
import Debug.Trace (trace)
import Text.Printf (printf)

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
dispatch (Pop n) = pop n
dispatch (Update n) = update n
dispatch Unwind = unwind
dispatch (Alloc n) = alloc n
dispatch (Slide n) = slide n

pushGlobal :: Name -> GmState -> GmState
pushGlobal f state =
  setStack (a : stack state) state
  where a = lookup (globals state) f (error ("Undeclared global " ++ f))

pushInt :: Int -> GmState -> GmState
pushInt n state = do
  let numName = show n
  case find (\(n, a) -> n == numName) (globals state) of
    Just (n, a) -> setStack (a : stack state) state
    Nothing ->
      setGlobals ((numName, a) : globals state)
        (setStack (a : stack state)
          (setHeap heap' state))
    where (heap', a) = heapAlloc (heap state) (Num n)

makeApplication :: GmState -> GmState
makeApplication state =
  setHeap heap' (setStack (a : as') state)
  where
    (heap', a) = heapAlloc (heap state) (Application a1 a2)
    (a1 : a2 : as') = stack state

push :: Int -> GmState -> GmState
push n state = setStack (as !! n : as) state
  where as = stack state

getArg :: Node -> Addr
getArg (Application a1 a2) = a2

pop :: Int -> GmState -> GmState
pop n state =
  setStack (drop n (stack state)) state

update :: Int -> GmState -> GmState
update n state = do
  let (a : as) = stack state
  let h = heapUpdate (heap state) (as !! n) (Indirect a)
  setStack as (setHeap h state)

unwind :: GmState -> GmState
unwind state =
  let node = heapLookup (heap state) a in
  case node of
    Indirect addr -> setStack (addr : as) state
    _ -> newState node state
  where (a : as) = stack state

alloc :: Int -> GmState -> GmState
alloc n state =
  let (h', as) = allocNodes n (heap state) in
    setStack (as ++ stack state) (setHeap h' state)

slide :: Int -> GmState -> GmState
slide n state = setStack (a : drop n as) state
  where (a : as) = stack state

newState :: Node -> GmState -> GmState
newState (Global n code) state
  | length (stack state) < n = error "Unwinding with too few arguments"
  | otherwise = setStack (rearrange n (heap state) (stack state)) (setCode code state)

newState (Num n) state = state
newState (Application a1 a2) state = setCode [Unwind] (setStack (a1 : stack state) state)

rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap stack = take n stack' ++ drop n stack
  where stack' = map (getArg . heapLookup heap) (tail stack)

allocNodes :: Int -> GmHeap -> (GmHeap, [Addr])
allocNodes 0 heap = (heap, [])
allocNodes n heap = (heap2, a : as)
  where
    (heap1, as) = allocNodes (n - 1) heap
    (heap2, a) = heapAlloc heap1 Uninit