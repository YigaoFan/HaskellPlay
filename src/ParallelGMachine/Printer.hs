module ParallelGMachine.Printer where

import ParallelGMachine.Util
    ( GlobalState(heap, output, stats, sparks),
      PGMachineState(globalState, PGMachineState),
      GmSparks,
      PGMachineStats,
      LocalState(..),
      GmClock,
      GlobalState(globals), global )
import AST (Name)
import Heap (Addr, heapLookup)
import PrettyPrint (Sequence (Newline), concat, num, str, interleave, layn, display, displaySeqs, laynAsSeqs, showItems)
import Prelude hiding (concat)
import GMachine.Util (Node(Global), GmOutput, GmVStack)
import GMachine.Printer (showInstructions, showDump, showStack, showSuperCombinator, showVStack, showOutput)

showResults :: [PGMachineState] -> [Char]
showResults states@(s : remain) =
  displaySeqs (
    str "SuperCombinator definitions" : Newline :
    interleave Newline (map (showSuperCombinator (heap (globalState s))) (globals (globalState s))) : Newline : Newline :
    str "State transitions" : Newline :
    laynAsSeqs (map showState states) ++ [Newline, Newline,
    showStats (stats lastGlobal), Newline,
    str "Output:", showOutput (output lastGlobal)])
  where
    lastGlobal = globalState (last states)

showState :: PGMachineState -> Sequence
showState state@(PGMachineState global locals) = concat [
  showGlobalState global, Newline,
  layn (map (showLocalState global) locals), Newline
  ]

showGlobalState :: GlobalState -> Sequence
showGlobalState globalState = concat [
  showOutput (output globalState), Newline,
  showSparks (sparks globalState), Newline,
  showStats (stats globalState)
  ]

showLocalState :: GlobalState -> LocalState -> Sequence
showLocalState global local = concat [
  showStack (stack local) (globals global) (heap global), Newline,
  showDump (dump local), Newline,
  showVStack (vStack local), Newline,
  showClock (clock local)
  ]

showStats :: PGMachineStats -> Sequence
showStats stats = concat [str "Status: ", showItems num stats]

showSparks :: GmSparks -> Sequence
showSparks sparks = concat [str "Sparks: ", showItems num sparks]

showClock :: GmClock -> Sequence
showClock = num