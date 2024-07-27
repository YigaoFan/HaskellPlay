module TIM.Util where

import Heap (Addr, Heap, heapAlloc, heapLookup, heapUpdate, lookup, initHeap)
import AST (Name)
import Prelude hiding (lookup)

data TimState = TimState
  {
    code :: TimCode,
    framePtr :: FramePtr,
    dataFramePtr :: FramePtr,
    stack :: TimStack,
    valueStack :: TimValueStack,
    dump :: TimDump,
    heap :: TimHeap,
    codeStore :: CodeStore,
    stats :: TimStats,
    output :: String
  }

setCode :: TimCode -> TimState -> TimState
setCode ins state = state {code = ins}
setFramePtr :: FramePtr -> TimState -> TimState
setFramePtr framePtr state = state { framePtr = framePtr }
setDataFramePtr :: FramePtr -> TimState -> TimState
setDataFramePtr dataFramePtr state = state { dataFramePtr = dataFramePtr }
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
setOutput :: String -> TimState -> TimState
setOutput o state = state { output = o }

data Instruction = Take Int Int |
  Enter TimAddrMode |
  Push TimAddrMode |
  PushV ValueAddrMode |
  Return |
  Op Op |
  Cond [Instruction] [Instruction] |
  Move Int TimAddrMode |
  PushMarker Int |
  UpdateMarkers Int |
  Switch [(Int, [Instruction])] |
  ReturnConstructor Int |
  Print

data TimAddrMode = Arg Int |
  Label [Char] Int |
  Code TimCode |
  IntConst Int |
  Data Int
data ValueAddrMode = FramePtr | IntValueConst Int
intCode = [PushV FramePtr, Return]

data FramePtr = FrameAddr Addr |
  FrameInt Int |
  FrameNull
  deriving Show

type TimCode = [Instruction]
type TimStack = [Closure]
type Closure = (TimCode, FramePtr)
type TimHeap = Heap Frame
type Frame = [Closure]
type CodeStore = (FramePtr, [(Name, Int)])
type TimStats = (Int, Int)
type TimValueStack = [Int]
type TimDump = [(FramePtr, Int, TimStack)]

data Op = Add | Sub | Mul | Div | Neg | Gr | GrEq | Lt | LtEq | Eq | NotEq deriving (Eq, Show)

setupInitStack :: TimHeap -> TimCode -> Int -> (TimStack, TimHeap)
setupInitStack heap topCont reservedSlots = ([(topCont, fptr)], h)
  where (h, fptr) = allocateFrame heap (replicate reservedSlots ([], FrameNull))
-- initStack = [(topCont, frame)]
initValueStack = []
initDump = []

allocateFrame :: TimHeap -> Frame -> (TimHeap, FramePtr)
allocateFrame heap frame = (h, FrameAddr a)
  where (h, a) = heapAlloc heap frame

getClosure :: TimHeap -> FramePtr -> Int -> Closure
getClosure heap (FrameAddr addr) n =
  heapLookup heap addr !! (n - 1)
getClosure heap addr _ = error ("getClosure not support this addr: " ++ show addr)

updateClosure :: TimHeap -> FramePtr -> Int -> Closure -> TimHeap
updateClosure heap (FrameAddr addr) n closure =
  heapUpdate heap addr newFrame
  where
    frame = heapLookup heap addr
    newFrame = take (n - 1) frame ++ (closure : drop n frame)

globalClosureLookup :: TimHeap -> CodeStore -> Name -> Int -> Closure
globalClosureLookup heap (f, maps) name = getClosure heap f

initStats :: TimStats
initStats = (0, 0)

incStatSteps :: TimStats -> TimStats
incStatSteps (stepCount, maxStackDepth) = (stepCount + 1, maxStackDepth)

getStepsFromStats :: TimStats -> Int
getStepsFromStats (stepCount, _) = stepCount

recordStackDepth :: Int -> TimStats -> TimStats
recordStackDepth depth (stepCount, maxStackDepth) = (stepCount, max depth maxStackDepth)

maxStackDepth :: TimStats -> Int
maxStackDepth (_, d) = d

initOutput = ""

-- | k is position of headCont
genTopCont :: Int -> TimCode
genTopCont k = [
  Switch [
    (1, []),
    (2, [Move 1 (Data 1), Move 2 (Data 2), Push (Label "headCont" k), Enter (Arg 1)])
    ]
  ]
-- topCont :: TimCode
-- topCont = [
--   Switch [
--     (1, []),
--     (2, [Move 1 (Data 1), Move 2 (Data 2), Push (Label "headCont"), Enter (Arg 1)])
--     ]
--   ]

-- | k is position of topCont
genHeadCont :: Int -> TimCode
genHeadCont k = [
  Print,
  Push (Label "topCont" k),
  Enter (Arg 2)
  ]

allocateInitHeap :: [Name] -> [(Name, TimCode)] -> (TimHeap, CodeStore)
allocateInitHeap noNeedUpdateCodeNames codes = (heap, (globalFrameAddr, offsets))
  where
    indexedCodes = zip [1..] codes
    offsets = [(name, offset) | (offset, (name, code)) <- indexedCodes]
    closures = [prefixUpdateIfIsCAF name code offset | (offset, (name, code)) <- indexedCodes]
    (heap, globalFrameAddr) = allocateFrame initHeap closures
    prefixUpdateIfIsCAF name code offset | name `elem` noNeedUpdateCodeNames = (code, FrameNull)
    prefixUpdateIfIsCAF name code@(Take _ paraCount : _) offset
      | paraCount > 0 = (code, FrameNull)
    prefixUpdateIfIsCAF name code@(UpdateMarkers paraCount : _) offset
      | paraCount > 0 = (code, FrameNull)
    prefixUpdateIfIsCAF name code offset = (PushMarker offset : code, globalFrameAddr)