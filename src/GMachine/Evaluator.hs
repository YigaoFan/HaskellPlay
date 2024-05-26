module GMachine.Evaluator where
import GMachine.Util (Node(..), GmState (stats, code, globals, stack, heap), setStats, incStatSteps, setCode, Instruction (PushGlobal, PushInt, MakeApplication, Slide, Push, Unwind), setStack, setHeap, setGlobals)
import AST (Name)
import Heap (lookup, heapAlloc, heapLookup, Addr)
import Prelude hiding (lookup)
import Data.Foldable (find)

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
dispatch Unwind = unwind

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
push n state =
  setStack (a : as) state
  where
    as = stack state
    a = getArg (heapLookup (heap state) (as !! (n + 1)))

getArg :: Node -> Addr
getArg (Application a1 a2) = a2

slide :: Int -> GmState -> GmState
slide n state =
  setStack (a : drop n as) state
  where (a : as) = stack state

unwind :: GmState -> GmState
unwind state = newState (heapLookup (heap state) a) state
  where (a : _) = stack state

newState :: Node -> GmState -> GmState
newState (Global n code) state
  | length (stack state) < n = error "Unwinding with too few arguments"
  | otherwise = setCode code state
newState (Num n) state = state
newState (Application a1 a2) state = setCode [Unwind] (setStack (a1 : stack state) state)
