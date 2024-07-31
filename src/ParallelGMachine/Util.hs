module ParallelGMachine.Util where

import GMachine.Util (GmOutput, GmHeap, GmGlobals, GmStats, GmCode, GmStack, GmDump, GmVStack)
import Heap (Addr)

type GlobalState = (GmOutput, GmHeap, GmGlobals, GmSparks, GmStats)
type LocalState = (GmCode, GmStack, GmDump, GmVStack, GmClock)
type ParallelGMachineState = (GlobalState, LocalState)

type GmSparks = [Addr]
type GmClock = Int