module TIM.Evaluator where

import TIM.Util (TimState(..), incStatSteps, Instruction (..), setStack, setFramePtr, FramePtr (..), setHeap, setStats, setCode, TimAddrMode (..), intCode, getClosure, allocateFrame, codeLookup, TimCode, recordStackDepth, ValueAddrMode (..), setValueStack, Op (..), updateClosure)
import qualified Data.List as DL (take)
import Prelude hiding (lookup, take, return)
import Prelude (Bool(False))
import Heap (heapAlloc, heapLookup, lookup)
import Debug.Trace (trace)
import AST (Name)

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
codeOf (Arg i) state = fst (getClosure (heap state) (framePtr state) i)
codeOf (Label n) state = codeLookup (codeStore state) n
codeOf (Code code) state = code
codeOf (IntConst n) state = intCode

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

enterOnlySetCode :: TimAddrMode -> TimState -> TimState
enterOnlySetCode addr state =
  if not (null (code state))
    then codeShouldBeEmptyError
    else let is = codeOf addr state in
      setCode is state

pushVFramePtr :: TimState -> TimState
pushVFramePtr state =
  let FrameInt n = framePtr state in
    setValueStack (n : valueStack state) state

pushVIntValueConst :: Int -> TimState -> TimState
pushVIntValueConst n state =
  setValueStack (n : valueStack state) state

return :: TimState -> TimState
return state =
  if not (null (code state))
    then codeShouldBeEmptyError
    else let (i, f) = head stk in
      setFramePtr f
        (setCode i
          (setStack (tail stk) state))
      where stk = stack state

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