module TIM.Compiler where

import AST (CoreProgram, CoreSuperCombinator, Name, CoreExpr, Expr (..))
import TIM.Util (TimState (TimState), Instruction (Take, Enter, Push), TimAddrMode (..), FramePtr (FrameNull), initStack, initValueStack, initDump, initStats, TimCode)
import Heap (lookup, initHeap)
import Prelude hiding (lookup)
import CorePrelude (primitives, defs)

type TimEnvironment = [(Name, TimAddrMode)]

compile :: CoreProgram -> TimState
compile program = TimState [Enter (Label "main")] FrameNull initStack initValueStack initDump initHeap compiledScDefs initStats
  where
    -- scDefs = defs ++ primitives ++ program
    scDefs = defs ++ program
    initEnv = [(name, Label name) | (name, _, _) <- scDefs]
    compiledScDefs = map (`compileSuperCombinator` initEnv) scDefs
    names = map (\(n, _, _) -> n) program

compileSuperCombinator :: CoreSuperCombinator -> TimEnvironment -> (Name, TimCode)
compileSuperCombinator (name, paraNames, body) env =
  if n == 0 
    then (name, compileR body (zipWith (\name i -> (name, Arg i)) paraNames [1..] ++ env))
    else (name, Take n : compileR body (zipWith (\name i -> (name, Arg i)) paraNames [1..] ++ env))
  where n = length paraNames

compileR :: CoreExpr -> TimEnvironment -> TimCode
compileR (Application e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR e@(Num {}) env = [Enter (compileA e env)]
compileR e@(Var {}) env = [Enter (compileA e env)]
compileR e env = error ("compileR: cannot compile " ++ show e)

compileA :: CoreExpr -> TimEnvironment -> TimAddrMode
compileA (Var name) env = lookup env name (error ("Unknown variable: " ++ name))
compileA (Num n) env = IntConst n
compileA e env = Code (compileR e env)



