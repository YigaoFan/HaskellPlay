module ParallelGMachine.Evaluator where
import ParallelGMachine.Util
    ( PGMachineState(PGMachineState, globalState, localStates),
      LocalState(code, LocalState, clock),
      setCode,
      setClock,
      setGlobalState,
      sparks,
      setStats,
      GlobalState(stats),
      setLocalStates,
      setSparks,
      GMachineState,
      GlobalState(..),
      LocalState(..),
      setStack )
import Data.List (mapAccumL)
import Heap (Addr)
import GMachine.Util (GmState (GmState), Instruction (Parallel, Unwind))
import qualified GMachine.Util as GM (output, heap, code, stack, vStack, dump, stats, globals)
import qualified GMachine.Evaluator as GM (dispatch)

eval :: PGMachineState -> [PGMachineState]
eval state = state : remain
  where
    remain
      | final state = []
      | otherwise = eval nextState
    nextState = doAdmin (steps state)

final :: PGMachineState -> Bool
final state = null (localStates state) && null (sparks (globalState state))

steps :: PGMachineState -> PGMachineState
steps state@(PGMachineState global locals) = PGMachineState newGlobal newLocals
  where
    newTasks = [makeTask a | a <- sparks global]
    global' = setSparks [] global
    locals' = map tick (locals ++ newTasks)
    (newGlobal, newLocals) = mapAccumL step global' locals'

makeTask :: Addr -> LocalState
makeTask addr = LocalState [Unwind] [addr] [] [] 0

tick :: LocalState -> LocalState
tick local = setClock (clock local + 1) local

step :: GlobalState -> LocalState -> (GlobalState, LocalState)
step global local = dispatch i state
  where
    i : is = code local
    local' = setCode is local
    state = (global, local')

doAdmin :: PGMachineState -> PGMachineState
doAdmin state@(PGMachineState global locals) =
  setLocalStates local' (setGlobalState (setStats stats' global) state)
  where
    (local', stats') = foldr filter ([], stats global) locals
    filter local (newLocals, newStats)
      | null $ code local = (newLocals, clock local : newStats)
      | otherwise = (local : newLocals, newStats)

dispatch :: Instruction -> GMachineState -> (GlobalState, LocalState)
dispatch Parallel (global, local) = parallel global local
dispatch i (global, local) =
  (global { output = GM.output s, heap = GM.heap s, globals = GM.globals s }, local { code = GM.code s, stack = GM.stack s, dump = GM.dump s, vStack = GM.vStack s, clock = GM.stats s})
  where
    s = GM.dispatch i (GmState (output global) (code local) (stack local) (dump local) (vStack local) (heap global) (globals global) (clock local))

parallel :: GlobalState -> LocalState -> GMachineState
parallel global local =
  let a = head stk in
    (setSparks (a : sparks global) global, setStack (tail stk) local)
  where stk = stack local
