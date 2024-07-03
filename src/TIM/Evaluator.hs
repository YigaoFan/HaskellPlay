module TIM.Evaluator where

import TIM.Util (TimState(..), incStatSteps, Instruction (..), setStack, setFramePtr, FramePtr (..), setHeap, setStats, setCode, TimAddrMode (..), intCode, getClosure, allocateFrame, codeLookup, TimCode, recordStackDepth, ValueAddrMode (..), setValueStack, Op (..))
import qualified Data.List as DL (take)
import Prelude hiding (lookup, take, return)
import Prelude (Bool(False))
import Heap (heapAlloc, heapLookup, lookup)
import Debug.Trace (trace)

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
dispatch (Take n) = take n
dispatch (Push (Arg k)) = pushArg k
dispatch (Push (Label l)) = pushLabel l
dispatch (Push (Code i)) = pushCode i
dispatch (Push (IntConst n)) = pushIntConst n
dispatch (Enter (Label l)) = enterLabel l -- trace ("enter " ++ l)
dispatch (Enter (Arg k)) = enterArg k
dispatch (Enter (Code i)) = enterCode i
dispatch (Enter (IntConst n)) = enterIntConst n
dispatch (PushV FramePtr) = pushVFramePtr
dispatch (PushV (IntValueConst n)) = pushVIntValueConst n
dispatch Return = return
dispatch (Op Add) = primitive2OnValueStack (+)
dispatch (Op Sub) = primitive2OnValueStack (-)
dispatch (Op Mul) = primitive2OnValueStack (*)
dispatch (Op Div) = primitive2OnValueStack div
dispatch (Op Neg) = primitiveOnValueStack negate
dispatch (Cond code1 code2) = cond code1 code2

take :: Int -> TimState -> TimState
take n state =
  if length (stack state) < n
    then error "Too few args for Take instruction"
    else let (h, fPtr) = allocateFrame (heap state) cs in
      setFramePtr fPtr
        (setHeap h
          (setStack remain state))
      where
        cs = DL.take n (stack state)
        remain = drop n (stack state)

pushArg :: Int -> TimState -> TimState
pushArg k state = do
  let c = getClosure (heap state) (framePtr state) k
  setStack (c : stack state) state

labelNotFoundError label = error ("not found label: " ++ label)
pushLabel :: String -> TimState -> TimState
pushLabel label state = do
  let code = codeLookup (codeStore state) label
  setStack ((code, framePtr state) : stack state) state

pushCode :: TimCode -> TimState -> TimState
pushCode code state =
  setStack ((code, framePtr state) : stack state) state

pushIntConst :: Int -> TimState -> TimState
pushIntConst n state =
  setStack ((intCode, FrameInt n) : stack state) state

codeShouldBeEmptyError = error "code should be empty when do enter"
enterLabel :: String -> TimState -> TimState
enterLabel label state =
  if not (null (code state))
    then codeShouldBeEmptyError
    else let code = codeLookup (codeStore state) label in
      setCode code state

enterArg :: Int -> TimState -> TimState
enterArg k state = 
  if not (null (code state))
    then codeShouldBeEmptyError
    else do
      let (ik, fk) = getClosure (heap state) (framePtr state) k
      setCode ik (setFramePtr fk state)

enterCode :: TimCode -> TimState -> TimState
enterCode instructions state =
  if not (null (code state))
    then codeShouldBeEmptyError
    else setCode instructions state

enterIntConst :: Int -> TimState -> TimState
enterIntConst n state =
  if not (null (code state))
    then codeShouldBeEmptyError
    else setCode intCode (setFramePtr (FrameInt n) state)

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
