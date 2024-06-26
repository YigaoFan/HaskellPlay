module TIM.Evaluator where

import TIM.Util (TimState(..), incStatSteps, Instruction (Take, Push, Enter), setStack, setFramePtr, FramePtr (..), setHeap, setStats, setCode, TimAMode (..), intCode)
import qualified Data.List as DL (take)
import Prelude hiding (lookup, take)
import Prelude (Bool(False))
import Heap (heapAlloc, heapLookup, lookup)

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
final s = False

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
dispatch (Enter (Label l)) = enterLabel l
dispatch (Enter (Arg k)) = enterArg k
dispatch (Enter (Code i)) = enterCode i
dispatch (Enter (IntConst n)) = enterIntConst n

take :: Int -> TimState -> TimState
take n state = do
  let (h, a) = heapAlloc (heap state) xs
  setFramePtr (FrameAddr a)
    (setHeap h
      (setStack remain state))
  where
    xs = DL.take n (stack state)
    remain = drop n (stack state)

pushArg :: Int -> TimState -> TimState
pushArg k state = do
  let FrameAddr a = framePtr state
  let f = heapLookup (heap state) a
  setStack (f !! (k - 1) : stack state) state

labelNotFoundError label = error ("not found label: " ++ label)
pushLabel :: String -> TimState -> TimState
pushLabel label state = do
  let store = codeStore state
  let code = lookup store label (labelNotFoundError label)
  setStack ((code, framePtr state) : stack state) state

pushCode :: [Instruction] -> TimState -> TimState
pushCode code state =
  setStack ((code, framePtr state) : stack state) state

pushIntConst :: Int -> TimState -> TimState
pushIntConst n state =
  setStack ((intCode, FrameInt n) : stack state) state

enterLabel :: String -> TimState -> TimState
enterLabel label state =
  if not (null (code state))
    then error "code should be empty when do enter"
    else let code = lookup (codeStore state) label (labelNotFoundError label) in
      setCode code state

codeShouldBeEmptyError = error "code should be empty when do enter"
enterArg :: Int -> TimState -> TimState
enterArg k state = 
  if not (null (code state))
  then codeShouldBeEmptyError
  else do
    let FrameAddr a = framePtr state
    let f = heapLookup (heap state) a
    let (ik, fk) = f !! (k - 1)
    setCode ik (setFramePtr fk state)

enterCode :: [Instruction] -> TimState -> TimState
enterCode instructions state =
  if not (null (code state))
  then codeShouldBeEmptyError
  else setCode instructions state

enterIntConst :: Int -> TimState -> TimState
enterIntConst n state =
  if not (null (code state))
  then codeShouldBeEmptyError
  else setCode intCode (setFramePtr (FrameInt n) state)