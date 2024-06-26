module TIM.Util where

import Heap (Addr, Heap)
import AST (Name)

data TimState = TimState
  {
    code :: [Instruction],
    framePtr :: FramePtr,
    stack :: TimStack,
    valueStack :: TimValueStack,
    dump :: TimDump,
    heap :: TimHeap,
    codeStore :: CodeStore,
    stats :: TimStats
  }

setCode :: [Instruction] -> TimState -> TimState
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
  Enter TimAMode |
  Push TimAMode

data TimAMode = Arg Int |
  Label [Char] |
  Code [Instruction] |
  IntConst Int
intCode = []

data FramePtr = FrameAddr Addr |
  FrameInt Int |
  FrameNull

type TimStack = [Closure]
type Closure = ([Instruction], FramePtr)

data TimValueStack = DummyTimValueStack
data TimDump = DummyTimDump

type TimHeap = Heap Frame
type Frame = [Closure]

type CodeStore = [(Name, [Instruction])]

type TimStats = Int
incStatSteps s = s + 1
