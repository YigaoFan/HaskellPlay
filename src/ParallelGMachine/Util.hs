{-# LANGUAGE TemplateHaskell #-}
module ParallelGMachine.Util where

import GMachine.Util (GmOutput, GmHeap, GmGlobals, GmStats, GmCode, GmStack, GmDump, GmVStack)
import Heap (Addr)
import ParallelGMachine.SetterTH (defineSetter)
import Language.Haskell.TH ()

data GlobalState = GloablState
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
data GMachineState = GMachineState
  {
    global :: GlobalState,
    local :: LocalState
  }
type GmSparks = [Addr]
type GmClock = Int
type PGMachineStats = [Int]

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
$(defineSetter "global")
$(defineSetter "local")
