module GMachine.Evaluator where
import GMachine.Util (Node(..), GmState (..), setStats, incStatSteps, setCode, Instruction (..), setStack, setHeap, setGlobals, GmHeap, GmStack, setDump, GmCode, nodeOfTopStack, setOutput, setVStack)
import AST (Name)
import Heap (lookup, heapAlloc, heapLookup, Addr, heapUpdate)
import Prelude hiding (print, lookup, return)
import Data.Foldable (find)
import Debug.Trace (trace)
import Text.Printf (printf)
import Data.List (intersperse)

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
dispatch Eval = evalNode
dispatch Add = arithmetic2 (+)
dispatch Sub = arithmetic2 (-)
dispatch Mul = arithmetic2 (*)
dispatch Div = arithmetic2 div
dispatch Neg = arithmetic1 negate
dispatch Eq = comparison (==)
dispatch Ne = comparison (/=)
dispatch Lt = comparison (<)
dispatch Le = comparison (<=)
dispatch Gt = comparison (>)
dispatch Ge = comparison (>=)
dispatch (Cond code1 code2) = cond code1 code2
dispatch (Pack t n) = pack t n
dispatch (CaseJump cases) = casejump cases
dispatch (Split n) = split n
dispatch Print = print
dispatch (PushBasic n) = pushBasic n
dispatch MakeBool = makeBool
dispatch MakeInt = makeInt
dispatch Get = get
dispatch Return = return

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

-- | push the arg of nth postion's application into stack
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
  if null (code state)
    then handle (heapLookup (heap state) a)
    else error "code while doing unwind is not empty"
  where
    a : as = stack state
    (i, s, v) : d = dump state
    handle (Indirect addr) = setCode [Unwind] (setStack (addr : as) state)
    handle (Num _)         = setDump d (setCode i (setStack (a : s) (setVStack v state)))
    handle (String _)      = setDump d (setCode i (setStack (a : s) (setVStack v state)))
    handle (Construct {})  = setDump d (setCode i (setStack (a : s) (setVStack v state)))
    handle (Global n _)
      | length (stack state) - 1 < n = setDump d (setCode i (setStack (last (stack state) : s) (setVStack v state)))
    handle n               = newState n state

alloc :: Int -> GmState -> GmState
alloc n state =
  let (h', as) = allocNodes n (heap state) in
    setStack (as ++ stack state) (setHeap h' state)

slide :: Int -> GmState -> GmState
slide n state = setStack (a : drop n as) state
  where (a : as) = stack state

newState :: Node -> GmState -> GmState
newState (Global n code) state
  | length (stack state) - 1 < n = error (printf "Unwinding with too few arguments. expect: %d, actual: %d" n (length (stack state)))
  | otherwise = setStack (rearrange n (heap state) (stack state)) (setCode code state) -- the original code should empty

-- newState (Num n) state = state
-- newState (String _) state = state
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

boxInteger :: Int -> GmState -> GmState
boxInteger n state =
  setStack (a : stack state) (setHeap h' state)
  where (h', a) = heapAlloc (heap state) (Num n)

unboxInteger :: Addr -> GmState -> Int
unboxInteger addr state =
  unbox (heapLookup (heap state) addr)
  where
    unbox (Num i) = i
    unbox n = error (printf "Unboxing a non-integer: %s" (show n))

boxBool :: Bool -> GmState -> GmState
boxBool b state =
  trace (printf "box bool at %d" a) setStack (a : stack state) (setHeap h' state)
  where
    (h', a) = heapAlloc (heap state) (Construct t [])
    t | b = 2 -- 2 is tag of True
      | otherwise = 1 -- 1 is tag of False

unboxBool :: Addr -> GmState -> Bool
unboxBool addr state =
  trace (printf "unbox bool at %d" addr) unbox (heapLookup (heap state) addr)
  where
    -- i = 1 :: Int
    unbox (Construct 2 []) = True
    unbox (Construct 1 []) = False
    unbox n = error (printf "Unboxing a non-boolean: %s" (show n))

primitive1 :: (b -> GmState -> GmState) -> (Addr -> GmState -> a) -> (a -> b) -> (GmState -> GmState)
primitive1 box unbox op state =
  box (op (unbox a state)) (setStack as state)
  where a : as = stack state

primitive1OnVStack :: (Int -> Int) -> (GmState -> GmState)
primitive1OnVStack op state =
  setVStack (op a : as) state
  where a : as = vStack state

primitive2 :: (b -> GmState -> GmState) -> (Addr -> GmState -> a) -> (a -> a -> b) -> (GmState -> GmState)
primitive2 box unbox op state =
  box (op (unbox a1 state) (unbox a2 state)) (setStack as state)
  where a1 : a2 : as = vStack state

primitive2OnVStack :: (Int -> Int -> Int) -> (GmState -> GmState)
primitive2OnVStack op state =
   setVStack (op a1 a2 : as) state
  where a1 : a2 : as = vStack state

-- 改下面三个
arithmetic1 :: (Int -> Int) -> (GmState -> GmState)
arithmetic1 = primitive1OnVStack

arithmetic2 :: (Int -> Int -> Int) -> (GmState -> GmState)
arithmetic2 = primitive2OnVStack

comparison :: (Int -> Int -> Bool) -> (GmState -> GmState)
comparison op = primitive2OnVStack (\a b -> if op a b then 2 else 1)

evalNode :: GmState -> GmState
evalNode state =
  setCode [Unwind]
    (setVStack []
      (setStack [a]
        (setDump ((code state, as, vStack state) : dump state) state)))
  where
    a : as = stack state

cond :: GmCode -> GmCode -> GmState -> GmState
cond code1 code2 state =
  let a : as = vStack state in
    setCode ((case a of
      2 -> code1
      1 -> code2) ++ code state)
        (setVStack as state)

pack :: Int -> Int -> GmState -> GmState
pack tag count state =
  setStack (a : drop count st) (setHeap h state)
  where
    st = stack state
    coms = take count st
    (h, a) = heapAlloc (heap state) (Construct tag coms)

casejump :: [(Int, GmCode)] -> GmState -> GmState
casejump cases state =
  let c = lookup cases tag (error (printf "not find corresponding tag %d" tag)) in
    setCode (c ++ code state) state
  where
    Construct tag _ = nodeOfTopStack state

split :: Int -> GmState -> GmState
split n state =
  setStack (coms ++ drop 1 (stack state)) state
  where
    Construct _ coms = nodeOfTopStack state

print :: GmState -> GmState
print state = -- reverse this time output content
  handle (nodeOfTopStack state)
  where
    handle (Num n) = popStackTop (setOutput (reverse (show n) ++ output state) state)
    handle (String s) = popStackTop (setOutput (reverse s ++ output state) state)
    handle (Construct t coms) =
      let printElems = intersperse comma coms ++ [rightParen] in
      setCode (concat (replicate (length printElems) [Eval, Print]) ++ code state) -- + 1 for print ')'
        (setStack (printElems ++ drop 1 (stack state))
          (setOutput ('(' : output state)
            (setHeap heap'' state)))
    handle e = error ("print not handled " ++ show e)
    (heap', rightParen) = heapAlloc (heap state) (String ")")
    (heap'', comma) = heapAlloc heap' (String " ,") --because output will be reversed, so ", " -> ""

    popStackTop s = setStack (drop 1 (stack s)) s

pushBasic :: Int -> GmState -> GmState
pushBasic num state =
  let vStk = vStack state in
    setVStack (num : vStk) state

-- set heap stack vStack h s v
-- | pack head of vStack item as Boolean into heap, then push the addr to stack
makeBool :: GmState -> GmState
makeBool state = do
  let t : vStk = vStack state
  let (h, a) = heapAlloc (heap state) (Construct t [])
  setStack (a : stack state) (setHeap h (setVStack vStk state))

-- | pack head of vStack item as Int into heap, then push the addr to stack
makeInt :: GmState -> GmState
makeInt state = do
  let n : vStk = vStack state
  let (h, a) = heapAlloc (heap state) (Num n)
  setStack (a : stack state) (setHeap h (setVStack vStk state))

get :: GmState -> GmState
get state =
  let a : stk = stack state in
    setStack stk
      (setVStack
        ((case heapLookup (heap state) a of
          Num n -> n
          Construct t [] -> t): vStack state) state)

return :: GmState -> GmState
return state =
  if not (null (code state))
    then error "code while doing unwind is not empty"
    else setCode i
      (setStack (head (stack state) : s)
        (setVStack v state))
      where
        (i, s, v) : d = dump state