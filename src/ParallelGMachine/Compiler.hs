module ParallelGMachine.Compiler where

import ParallelGMachine.Util (PGMachineState(..), LocalState(..), GlobalState(..))
import AST (CoreProgram)
import Heap (lookup, Addr)
import GMachine.Util (Instruction(Print, Eval))
import GMachine.Compiler (buildInitHeap)
import Prelude hiding (lookup)

compile :: CoreProgram -> PGMachineState
compile program =
  PGMachineState (GlobalState "" heap globals [] []) [initTask addr]
  where
    (heap, globals) = buildInitHeap program
    addr = lookup globals "main" (error "main entry undefined")

initTask :: Addr -> LocalState
initTask addr = LocalState initialCode [addr] [] [] 0

initialCode = [Eval, Print]
