module ParallelGMachine.Evaluator where
import ParallelGMachine.Util
    ( PGMachineState(PGMachineState, globalState, localStates),
      LocalState(code, LocalState, clock),
      setCode,
      setClock,
      GMachineState(GMachineState),
      setGlobalState,
      sparks,
      setStats,
      GlobalState(stats),
      setLocalStates,
      setSparks
      )
import Data.List (mapAccumL)
import Heap (Addr)
import GMachine.Util (Instruction(Unwind))

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
    state = GMachineState global local'

doAdmin :: PGMachineState -> PGMachineState
doAdmin state@(PGMachineState global locals) =
  setLocalStates local' (setGlobalState (setStats stats' global) state)
  where
    (local', stats') = foldr filter ([], stats global) locals
    filter local (newLocals, newStats)
      | null $ code local = (newLocals, clock local : newStats)
      | otherwise = (local : newLocals, newStats)

dispatch = error "not implement"

par :: GMachineState -> GMachineState
par s = s
