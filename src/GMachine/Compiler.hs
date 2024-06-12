module GMachine.Compiler where
import AST (CoreProgram, CoreSuperCombinator, Name, CoreExpr, Expr (Var, Num, Application, Let))
import GMachine.Util (GmState (GmState), GmCode, Instruction (..), initialStat, GmHeap, GmGlobals, Node (Global), GmEnvironment, domain)
import Heap (initHeap, Addr, heapAlloc, lookup)
import Data.List (mapAccumL)
import Prelude hiding (lookup)
import CorePrelude (defs, extraDefs)
import Debug.Trace (trace)

initialCode :: GmCode
initialCode = [PushGlobal "main", Eval]

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

compile :: CoreProgram -> GmState
compile program = GmState initialCode [] [] heap globals initialStat
  where (heap, globals) = buildInitHeap program

type GmCompiledSuperCombinator = (Name, Int, GmCode)
compiledPrimitives :: [GmCompiledSuperCombinator]
compiledPrimitives =
  [
    ("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
    ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),
    ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),
    ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind]),
    ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind]),
    ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),
    ("/=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind]),
    ("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind]),
    ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind]),
    (">", 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind]),
    (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind]),
    ("if", 2, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])
  ]
buildInitHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitHeap program = mapAccumL allocSuperCombinator initHeap (map compileSuperCombinator (defs ++ program) ++ compiledPrimitives)

allocSuperCombinator :: GmHeap -> GmCompiledSuperCombinator -> (GmHeap, (Name, Addr))
allocSuperCombinator heap (name, argCount, instructions) =
  (heap', (name, addr))
  where (heap', addr) = heapAlloc heap (Global argCount instructions)

compileSuperCombinator :: CoreSuperCombinator -> GmCompiledSuperCombinator
compileSuperCombinator (name, paraNames, body) =
  (name, length paraNames, compileR body (zip paraNames [0..]))

compileR :: GmCompiler
compileR exp env = compileC exp env ++ [Update (length env), Pop (length env), Unwind] --为什么这里需要 pop？

argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(name, offset + n) | (name, offset) <- env]

compileC :: GmCompiler
compileC (Var name) env
  | name `elem` domain env = [Push (lookup env name (error "impossible"))]
  | otherwise = [PushGlobal name]
compileC (Num n) env = [PushInt n]
compileC (Application e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MakeApplication]
compileC (Let False defs exp) env = compileLet compileC defs exp env
compileC (Let True defs exp) env = compileLetrec compileC defs exp env

compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLetrec compile defs exp env =
  Alloc n :
  concat (zipWith (\i e -> compile (snd e) env' ++ [Update i]) [n - 1 .. 0] defs) ++
  compile exp env' ++
  [Slide n]
  where
    n = length defs
    env' = compileArgs defs env
compileLet :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler
compileLet compile defs exp env =
  compileLetDefs defs env ++ compile exp env' ++ [Slide (length defs)]
  where env' = compileArgs defs env
compileLetDefs :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLetDefs [] env = []
compileLetDefs ((name, exp) : defs) env =
  compileC exp env ++ compileLetDefs defs (argOffset 1 env)
compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env = zip (map fst defs) [n - 1, n - 2 .. 0] ++ argOffset n env
  where n = length defs