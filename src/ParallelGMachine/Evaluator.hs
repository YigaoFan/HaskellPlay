module ParallelGMachine.Evaluator where
import ParallelGMachine.Util (ParallelGMachineState)

eval :: ParallelGMachineState -> [ParallelGMachineState]
eval state = state : remain
  where
    remain
      | final state = []
      | otherwise = eval nextState
    nextState = doAdmin (step state)

final :: ParallelGMachineState -> Bool
final state = True

step :: ParallelGMachineState -> ParallelGMachineState
