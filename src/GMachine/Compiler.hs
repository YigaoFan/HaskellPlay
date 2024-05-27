module GMachine.Compiler where
import AST (CoreProgram, CoreSuperCombinator, Name, CoreExpr, Expr (Var, Num, Application))
import GMachine.Util (GmState (GmState), GmCode, Instruction (PushGlobal, Unwind, Push, PushInt, MakeApplication, Update, Pop), initialStat, GmHeap, GmGlobals, Node (Global), GmEnvironment, domain)
import Heap (initHeap, Addr, heapAlloc, lookup)
import Data.List (mapAccumL)
import Prelude hiding (lookup)
import CorePrelude (defs, extraDefs)

initialCode :: GmCode
initialCode = [PushGlobal "main", Unwind]

compile :: CoreProgram -> GmState
compile program = GmState initialCode [] heap globals initialStat
  where (heap, globals) = buildInitHeap program

buildInitHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitHeap program = mapAccumL allocSuperCombinator initHeap (map compileSuperCombinator (defs ++ program))

type GmCompiledSuperCombinator = (Name, Int, GmCode)
allocSuperCombinator :: GmHeap -> GmCompiledSuperCombinator -> (GmHeap, (Name, Addr))
allocSuperCombinator heap (name, argCount, instructions) =
  (heap', (name, addr))
  where (heap', addr) = heapAlloc heap (Global argCount instructions)

compileSuperCombinator :: CoreSuperCombinator -> GmCompiledSuperCombinator
compileSuperCombinator (name, paraNames, body) =
  (name, length paraNames, compileR body (zip paraNames [0..]))

compileR :: CoreExpr -> GmEnvironment -> GmCode
compileR exp env = compileC exp env ++ [Update (length env), Pop (length env), Unwind]

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(name, offset + n) | (name, offset) <- env]

compileC :: CoreExpr -> GmEnvironment -> GmCode
compileC (Var name) env
  | name `elem` domain env = [Push (lookup env name (error "impossible"))]
  | otherwise = [PushGlobal name]
compileC (Num n) env = [PushInt n]
compileC (Application e1 e2) env = compileC e1 env ++ compileC e2 (argOffset 1 env) ++ [MakeApplication]