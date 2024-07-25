module TIM.Evaluator where

import TIM.Util (TimState(..), incStatSteps, Instruction (..), setStack, setFramePtr, FramePtr (..), setHeap, setStats, setCode, TimAddrMode (..), intCode, getClosure, allocateFrame, codeLookup, TimCode, recordStackDepth, ValueAddrMode (..), setValueStack, Op (..), updateClosure, setDump, setDataFramePtr, setOutput)
import qualified Data.List as DL (take)
import Prelude hiding (lookup, take, return, print)
import Prelude (Bool(False))
import Heap (heapAlloc, heapLookup, lookup)
import Debug.Trace (trace, traceStack)
import AST (Name)
import GHC.Stack (HasCallStack)

eval :: TimState -> [TimState]
eval state = state : remain
  where
    remain
      | final state = []
      | otherwise = eval nextState
    nextState = doAdmin (step state)

doAdmin :: TimState -> TimState
doAdmin s = setStats (recordStackDepth (length (stack s)) (incStatSteps (stats s))) s

final :: TimState -> Bool
final s = null (code s)

step :: TimState -> TimState
step state = dispatch i (setCode is state)
  where
    (i : is) = code state

dispatch :: Instruction -> TimState -> TimState
dispatch (Take cap n) = take cap n
dispatch (Move i addr) = move i addr
dispatch (Push addr) = push addr
dispatch (Enter addr@(Arg k)) = enter addr
dispatch (Enter addr@(IntConst n)) = enter addr
dispatch (Enter addr@(Label l)) = enterOnlySetCode addr
dispatch (Enter addr@(Code i)) = enterOnlySetCode addr
dispatch (PushV FramePtr) = pushVFramePtr
dispatch (PushV (IntValueConst n)) = pushVIntValueConst n
dispatch Return = return
dispatch (Op Add) = primitive2OnValueStack (+)
dispatch (Op Sub) = primitive2OnValueStack (-)
dispatch (Op Mul) = primitive2OnValueStack (*)
dispatch (Op Div) = primitive2OnValueStack div
dispatch (Op Neg) = primitiveOnValueStack negate
dispatch (Op Lt) = primitive2OnValueStack (\a -> bool2Int . (<) a)
dispatch (Op LtEq) = primitive2OnValueStack (\a -> bool2Int . (<=) a)
dispatch (Op Gr) = primitive2OnValueStack (\a -> bool2Int . (>) a)
dispatch (Op GrEq) = primitive2OnValueStack (\a -> bool2Int . (>=) a)
dispatch (Op Eq) = primitive2OnValueStack (\a -> bool2Int . (==) a)
dispatch (Op NotEq) = primitive2OnValueStack (\a -> bool2Int . (/=) a)
dispatch (Cond code1 code2) = cond code1 code2
dispatch (PushMarker x) = pushMarker x
dispatch (UpdateMarkers n) = updateMarkers n
dispatch (Switch alts) = switch alts
dispatch (ReturnConstructor t) = returnConstructor t
dispatch Print = print

take :: Int -> Int -> TimState -> TimState
take cap n state
  | length (stack state) < n = error "Too few args for Take instruction"
  | n > cap = error "Wrong take operation: n is bigger than capacity"
  | otherwise =
    let (h, fPtr) = allocateFrame (heap state) cs in
      setFramePtr fPtr
        (setHeap h
          (setStack remain state))
  where
      cs = DL.take n (stack state) ++ replicate (cap - n) ([], FrameNull)
      remain = drop n (stack state)

closureOf (Arg i) state = getClosure (heap state) (framePtr state) i
closureOf (Label n) state = (codeLookup (codeStore state) n, framePtr state)
closureOf (Code code) state = (code, framePtr state)
closureOf (IntConst n) state = (intCode, FrameInt n)
closureOf (Data i) state = getClosure (heap state) (dataFramePtr state) i

move :: Int -> TimAddrMode -> TimState -> TimState
move to addr state =
  setHeap (updateClosure (heap state) (framePtr state) to (closureOf addr state)) state

push :: TimAddrMode -> TimState -> TimState
push addr state =
  let c = closureOf addr state in
    setStack (c : stack state) state

codeShouldBeEmptyError = error "code should be empty when do enter"
enter :: TimAddrMode -> TimState -> TimState
enter addr state =
   if not (null (code state))
    then codeShouldBeEmptyError
    else let (is, f) = closureOf addr state in
      setCode is (setFramePtr f state)

-- | only for TimAddrMode(Code, IntConst) 
enterOnlySetCode :: TimAddrMode -> TimState -> TimState
enterOnlySetCode addr state =
  if not (null (code state))
    then codeShouldBeEmptyError
    else setCode (codeOf addr state) state
  where
    codeOf (Code code) state = code
    codeOf (Label n) state = codeLookup (codeStore state) n

pushVFramePtr :: TimState -> TimState
pushVFramePtr state =
  let FrameInt n = framePtr state in
    setValueStack (n : valueStack state) state

pushVIntValueConst :: Int -> TimState -> TimState
pushVIntValueConst n state =
  setValueStack (n : valueStack state) state

return :: TimState -> TimState
return state
  | not (null (code state)) = codeShouldBeEmptyError
  | null (valueStack state) = error "value stack should not empty when return"
  | null stk = do
    let (f, x, s) = head dmp
    let h' = updateClosure (heap state) f x (intCode, FrameInt (head (valueStack state)))
    setHeap h' (setDump (tail dmp) (setStack s (setCode [Return] state)))
  | otherwise = let (i, f) = head stk in
    setFramePtr f
      (setCode i
        (setStack (tail stk) state))
  where
    stk = stack state
    dmp = dump state

primitiveOnValueStack :: (Int -> Int) -> (TimState -> TimState)
primitiveOnValueStack op state =
  setValueStack (op a : as) state
  where
    a : as = valueStack state

primitive2OnValueStack :: (Int -> Int -> Int) -> (TimState -> TimState)
primitive2OnValueStack op state =
  setValueStack (op a1 a2 : as) state
  where a1 : a2 : as = valueStack state

cond :: TimCode -> TimCode -> TimState -> TimState
cond code1 code2 state =
  if not (null (code state))
    then codeShouldBeEmptyError
    else let a : as = valueStack state in
      (if a == 0
        then setCode code1
        else setCode code2)
        (setValueStack as state)

bool2Int :: Bool -> Int
bool2Int b = if b then 0 else 1

-- | set a update marker for the future
pushMarker :: Int -> TimState -> TimState
pushMarker x state =
  setDump ((f, x, stk) : dump state) (setStack [] state)
  where
    stk = stack state
    f = framePtr state

-- | extend stack and update closure when args is not enough
updateMarkers :: Int -> TimState -> TimState
updateMarkers n state
  | length stk >= n = state
  | otherwise =
    setHeap h''
      (setStack (stk ++ s)
        (setCode (UpdateMarkers n : code state)
          (setDump (tail dmp) state)))
  where
    dmp = dump state
    stk = stack state
    m = length stk
    (f, x, s) = head dmp
    (h', f') = allocateFrame (heap state) stk
    h'' = updateClosure h' f x (map (Push . Arg) [m, m - 1 .. 1] ++ UpdateMarkers n : code state, f')

switch :: [(Int, TimCode)] -> TimState -> TimState
switch alts state
  | not (null (code state)) = codeShouldBeEmptyError
  | otherwise = let t = head (valueStack state) in
    setCode (lookup alts t (error ("not found alt for tag " ++ show t)))
      (setValueStack (tail (valueStack state)) state)

returnConstructor :: Int -> TimState -> TimState
returnConstructor tag state
  | not (null (code state)) = codeShouldBeEmptyError
  | null stk = let (fu, x, s) = head dmp in
    let h = updateClosure (heap state) fu x ([ReturnConstructor tag], framePtr state) in
      setCode [ReturnConstructor tag]
        (setStack s
          (setHeap h 
            (setDump (tail dmp) state)))
  | otherwise = let (is, f) = head stk in
      setCode is 
        (setFramePtr f
          (setDataFramePtr (framePtr state)
            (setStack (tail stk)
              (setValueStack (tag : valueStack state) state))))
  where
    dmp = dump state
    stk = stack state

print :: TimState -> TimState
print state = setOutput (output state ++ show (head vs)) state
  where vs = valueStack state
