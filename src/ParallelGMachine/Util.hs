{-# LANGUAGE TemplateHaskell #-}
module ParallelGMachine.Util where

import GMachine.Util (GmOutput, GmHeap, GmGlobals, GmStats, GmCode, GmStack, GmDump, GmVStack, Instruction)
import Heap (Addr)
import ParallelGMachine.SetterTH (defineSetter)

data GlobalState = GlobalState
  {
    output :: GmOutput,
    heap :: GmHeap,
    globals :: GmGlobals,
    sparks :: GmSparks,
    stats :: PGMachineStats
  }
data LocalState = LocalState 
  {
    code :: GmCode,
    stack :: GmStack,
    dump :: GmDump,
    vStack :: GmVStack, 
    clock :: GmClock
  }
data PGMachineState = PGMachineState
  {
    globalState :: GlobalState,
    localStates :: [LocalState]
  }
type GMachineState = (GlobalState, LocalState) -- mainly for adapting GMachine
type GmSparks = [Addr]
type GmClock = Int
type PGMachineStats = [Int]
-- data ParallelInstruction = ProcessorInstr Instruction | Parallel
-- type ParallelCode = [ParallelInstruction]
$(defineSetter "output")
$(defineSetter "heap")
$(defineSetter "globals")
$(defineSetter "sparks")
$(defineSetter "stats")
$(defineSetter "code")
$(defineSetter "stack")
$(defineSetter "vStack")
$(defineSetter "clock")
$(defineSetter "globalState")
$(defineSetter "localStates")
global :: GMachineState -> GlobalState
global (g, _) = g
local :: GMachineState -> LocalState
local (_, l) = l
setGlobal :: GlobalState -> GMachineState -> GMachineState
setGlobal g (_, l) = (g, l)
setLocal :: LocalState -> GMachineState -> GMachineState
setLocal l (g, _) = (g, l)
