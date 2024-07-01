module TIM.Util where

import Heap (Addr, Heap, heapAlloc, heapLookup, heapUpdate, lookup)
import AST (Name)
import Prelude hiding (lookup)

data TimState = TimState
  {
    code :: TimCode,
    framePtr :: FramePtr,
    stack :: TimStack,
    valueStack :: TimValueStack,
    dump :: TimDump,
    heap :: TimHeap,
    codeStore :: CodeStore,
    stats :: TimStats
  }

setCode :: TimCode -> TimState -> TimState
setCode ins state = state {code = ins}
setFramePtr :: FramePtr -> TimState -> TimState
setFramePtr framePtr state = state { framePtr = framePtr }
setStack :: TimStack -> TimState -> TimState
setStack stack state = state { stack = stack }
setValueStack :: TimValueStack -> TimState -> TimState
setValueStack valueStack state = state { valueStack = valueStack }
setDump :: TimDump -> TimState -> TimState
setDump dump state = state { dump = dump }
setHeap :: TimHeap -> TimState -> TimState
setHeap heap state = state { heap = heap }
setCodeStore :: CodeStore -> TimState -> TimState
setCodeStore codeStore state = state { codeStore = codeStore }
setStats :: TimStats -> TimState -> TimState
setStats stats state = state { stats = stats }

data Instruction = Take Int |
  Enter TimAddrMode |
  Push TimAddrMode

data TimAddrMode = Arg Int |
  Label [Char] |
  Code TimCode |
  IntConst Int
intCode = []

data FramePtr = FrameAddr Addr |
  FrameInt Int |
  FrameNull

type TimCode = [Instruction]
type TimStack = [Closure]
type Closure = (TimCode, FramePtr)
type TimHeap = Heap Frame
type Frame = [Closure]
type CodeStore = [(Name, TimCode)]
type TimStats = Int
data TimValueStack = DummyTimValueStack
data TimDump = DummyTimDump

initStack = []
initValueStack = DummyTimValueStack
initDump = DummyTimDump
initStats :: Int
initStats = 0
incStatSteps s = s + 1

allocateFrame :: TimHeap -> Frame -> (TimHeap, FramePtr)
allocateFrame heap frame = (h, FrameAddr a)
  where (h, a) = heapAlloc heap frame

getClosure :: TimHeap -> FramePtr -> Int -> Closure
getClosure heap (FrameAddr addr) n =
  heapLookup heap addr !! (n - 1)

updateClosure :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
updateClosure heap (FrameAddr addr) n closure =
  heapUpdate heap addr newFrame
  where
    frame = heapLookup heap addr
    newFrame = take (n - 1) frame ++ (closure : drop n frame)

codeLookup :: CodeStore -> Name -> TimCode
codeLookup codeStore name =
  lookup codeStore name (error ("not found code for label " ++ name))

getStepsFromStats :: TimStats -> Int
getStepsFromStats stats = stats
