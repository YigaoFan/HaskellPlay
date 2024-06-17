module GMachine.Compiler where
import AST (CoreProgram, CoreSuperCombinator, Name, CoreExpr, Expr (Var, Num, Application, Let, Case, Constructor), Alter)
import GMachine.Util (GmState (GmState), GmCode, Instruction (..), initialStat, GmHeap, GmGlobals, Node (Global), GmEnvironment, domain)
import Heap (initHeap, Addr, heapAlloc, lookup)
import Data.List (mapAccumL)
import Prelude hiding (lookup)
import CorePrelude (defs, extraDefs)
import Debug.Trace (trace)
import Text.Printf (printf)

initialCode :: GmCode
initialCode = [PushGlobal "main", Eval, Print]

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

compile :: CoreProgram -> GmState
compile program = GmState "" initialCode [] [] heap globals initialStat
  where (heap, globals) = buildInitHeap program

type GmCompiledSuperCombinator = (Name, Int, GmCode)
compiledPrimitives :: [GmCompiledSuperCombinator]
compiledPrimitives = [
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
    ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind]) -- Update、Pop 是 Application 中参数个数
  ]

builtInDyadic :: [(Name, Instruction)]
builtInDyadic = [
  ("+", Add), ("-", Sub), ("*", Mul), ("/", Div),
  ("==", Eq), ("/=", Ne), ("/=", Ne), 
  (">", Gt), (">=", Ge), ("<", Lt),  ("<=", Le)
  ]

buildInitHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitHeap program = mapAccumL allocSuperCombinator initHeap (map compileSuperCombinator (program) ++ compiledPrimitives)

allocSuperCombinator :: GmHeap -> GmCompiledSuperCombinator -> (GmHeap, (Name, Addr))
allocSuperCombinator heap (name, argCount, instructions) =
  (heap', (name, addr))
  where (heap', addr) = heapAlloc heap (Global argCount instructions)

compileSuperCombinator :: CoreSuperCombinator -> GmCompiledSuperCombinator
compileSuperCombinator (name, paraNames, body) =
  (name, length paraNames, compileR body (zip paraNames [0..]))

compileR :: GmCompiler
compileR exp env = compileE exp env ++ [Update (length env), Pop (length env), Unwind] --为什么这里需要 pop？因为需要把 unwind 的对象露出来
-- 但这里为什么 Update 和 Pop 是一样的数字呢？如果 compileC exp 里 Push 东西到栈上呢，见笔记
argOffset :: Int -> GmEnvironment -> GmEnvironment
argOffset n env = [(name, offset + n) | (name, offset) <- env]

compileC :: GmCompiler
compileC (Var name) env
  | name `elem` domain env = [Push (lookup env name (error "impossible"))]
  | otherwise = [PushGlobal name]
compileC (Num n) env = [PushInt n]
compileC (Constructor t a) env = [Pack t a]
compileC app@(Application e1 e2) env -- 下面这个说明有这种语法：Pack{x, y} e1 e2 e3...
  | hasValidPack app = iter2Cons app env
  | otherwise = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [MakeApplication]
   where
    getPack n@(Constructor {}) argCount = Just (n, argCount)
    getPack (Application e1 _) argCount = getPack e1 (argCount + 1)
    getPack e _ = trace (printf "getPack: %s" (show e)) Nothing
    hasValidPack (Application e1 e2) = 
      case getPack e1 1 of
        Nothing -> trace "getPack: False" False
        Just (Constructor t a, argCount) -> a == argCount
    iter2Cons (Application e arg) currentEnv = compileC arg currentEnv ++ iter2Cons e (argOffset 1 currentEnv)
    iter2Cons (Constructor t a) _ = [Pack t a]

compileC (Let False defs exp) env = compileLet defs compileC exp env
compileC (Let True defs exp) env = compileLetrec compileC defs compileC exp env

compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler -> GmCompiler
compileLetrec compileDef defs compileExp exp env =
  Alloc n :
  concat (zipWith (\i e -> compileDef (snd e) env' ++ [Update i]) [n - 1 .. 0] defs) ++
  compileExp exp env' ++
  [Slide n]
  where
    n = length defs
    env' = compileArgs defs env
compileLet :: [(Name, CoreExpr)] -> GmCompiler -> GmCompiler
compileLet defs compileExp exp env =
  compileLetDefs defs env ++ compileExp exp env' ++ [Slide (length defs)]
  where env' = compileArgs defs env
compileLetDefs :: [(Name, CoreExpr)] -> GmEnvironment -> GmCode
compileLetDefs [] env = []
compileLetDefs ((name, exp) : defs) env =
  compileC exp env ++ compileLetDefs defs (argOffset 1 env)
compileArgs :: [(Name, CoreExpr)] -> GmEnvironment -> GmEnvironment
compileArgs defs env = zip (map fst defs) [n - 1, n - 2 .. 0] ++ argOffset n env
  where n = length defs

compileE :: GmCompiler
compileE (Num n) env = [PushInt n]
compileE (Let False defs exp) env = compileLet defs compileE exp env
compileE (Let True defs exp) env = compileLetrec compileC defs compileE exp env
compileE (Application (Application (Var op) e0) e1) env
  | op `elem` domain builtInDyadic = compileE e1 env ++ compileE e0 (argOffset 1 env) ++ [lookup builtInDyadic op (error "impossible")]
compileE (Application (Var "negate") e) env = compileE e env ++ [Neg]
compileE (Application (Application (Application (Var "if") e0) e1) e2) env =
  compileE e0 env ++ [Cond (compileE e1 env) (compileE e2 env)]
compileE (Case e alts) env = compileE e env ++ [CaseJump (compileAlts compileE' alts env)]
compileE e env = compileC e env ++ [Eval]

compileAlts :: (Int -> GmCompiler) -> [Alter Name] -> GmEnvironment -> [(Int, GmCode)]
compileAlts compile alts env = [(tag, compile (length names) body (zip names [0..] ++ argOffset (length names) env)) | (tag, names, body) <- alts]

compileE' :: Int -> GmCompiler
compileE' offset exp env = Split offset : compileE exp env ++ [Slide offset]
