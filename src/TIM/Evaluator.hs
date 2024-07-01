module TIM.Evaluator where

import TIM.Util (TimState(..), incStatSteps, Instruction (Take, Push, Enter), setStack, setFramePtr, FramePtr (..), setHeap, setStats, setCode, TimAddrMode (..), intCode, getClosure, allocateFrame, codeLookup, TimCode)
import qualified Data.List as DL (take)
import Prelude hiding (lookup, take)
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
doAdmin s = setStats (incStatSteps (stats s)) s

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
dispatch (Enter (Label l)) = trace ("enter " ++ l)  enterLabel l
dispatch (Enter (Arg k)) = enterArg k
dispatch (Enter (Code i)) = enterCode i
dispatch (Enter (IntConst n)) = enterIntConst n

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

-- 弄懂 frame 的运行过程