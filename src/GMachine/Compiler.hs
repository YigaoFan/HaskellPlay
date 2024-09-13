module GMachine.Compiler where
import AST (CoreProgram, CoreSuperCombinator, Name, CoreExpr, Expr (Var, Num, Application, Let, Case, Constructor), Alter)
import GMachine.Util (GmState (GmState), GmCode, Instruction (..), initialStat, GmHeap, GmGlobals, Node (Global), GmEnvironment, )
import Heap (initHeap, Addr, heapAlloc, lookup)
import Data.List (mapAccumL)
import Prelude hiding (lookup)
import CorePrelude (defs, extraDefs, primitives)
import Debug.Trace (trace)
import Text.Printf (printf)
import Util (domain)

initialCode :: GmCode
initialCode = [PushGlobal "main", Eval, Print]

type GmCompiler = CoreExpr -> GmEnvironment -> GmCode

compile :: CoreProgram -> GmState
compile program = GmState "" initialCode [] [] [] heap globals initialStat
  where (heap, globals) = buildInitHeap program

type GmCompiledSuperCombinator = (Name, Int, GmCode)

builtInDyadicBoolOperator :: [(Name, Instruction)]
builtInDyadicBoolOperator = [
  ("==", Eq), ("/=", Ne),
  (">", Gt), (">=", Ge), ("<", Lt),  ("<=", Le)
  ]
builtInDyadicNumOperator :: [(Name, Instruction)]
builtInDyadicNumOperator = [
  ("+", Add),
  ("-", Sub),
  ("*", Mul),
  ("/", Div)
  ]

buildInitHeap :: CoreProgram -> (GmHeap, GmGlobals)
buildInitHeap program = mapAccumL allocSuperCombinator initHeap (map compileSuperCombinator (program ++ primitives))

allocSuperCombinator :: GmHeap -> GmCompiledSuperCombinator -> (GmHeap, (Name, Addr))
allocSuperCombinator heap (name, argCount, instructions) =
  (heap', (name, addr))
  where (heap', addr) = heapAlloc heap (Global argCount instructions)

compileSuperCombinator :: CoreSuperCombinator -> GmCompiledSuperCombinator
compileSuperCombinator (name, paraNames, body) =
  (name, length paraNames, compileR body (zip paraNames [0..]))

compileR :: GmCompiler
compileR (Num n) env = [PushInt n, Return]
compileR e@(Application (Application (Var op) e0) e1) env
  | op `elem` domain builtInDyadicNumOperator = compileB e env ++ [MakeInt, Return]
  | op `elem` domain builtInDyadicBoolOperator = compileB e env ++ [MakeBool, Return]
compileR e@(Application (Var "negate") _) env = compileB e env ++ [MakeInt, Return]
compileR (Let False defs exp) env = compileLet defs compileR (const []) exp env
compileR (Let True defs exp) env = compileLetrec compileC defs compileR (const []) exp env
compileR (Application (Application (Application (Var "if") e0) e1) e2) env =
  compileB e0 env ++ [Cond (compileR e1 env) (compileR e2 env)]
compileR (Case e alts) env = compileE e env ++ [CaseJump (compileAlts compileRAltBody alts env)]
compileR (Application (Application (Var "par") e1) e2) env =
  compileC e2 env ++ [Push 0, Parallel] ++
  compileC e1 (argOffset 1 env) ++ [MakeApplication, Eval]
compileR exp env = compileE exp env ++ [Update (length env), Pop (length env), Unwind] --为什么这里需要 pop？因为需要把 unwind 的对象露出来

compileRAltBody :: Int -> GmCompiler
compileRAltBody offset exp env = Split offset : compileR exp env


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

compileC (Let False defs exp) env = compileLet defs compileC (\n -> [Slide n]) exp env
compileC (Let True defs exp) env = compileLetrec compileC defs compileC (\n -> [Slide n]) exp env
compileC e env = error ("not handled in compileC " ++ show e)

compileLetrec :: GmCompiler -> [(Name, CoreExpr)] -> GmCompiler -> (Int -> GmCode) -> GmCompiler
compileLetrec compileDef defs compileExp cleanStack exp env =
  Alloc n :
  concat (zipWith (\i e -> compileDef (snd e) env' ++ [Update i]) [n - 1, n -2 .. 0] defs) ++
  compileExp exp env' ++
  cleanStack n
  where
    n = length defs
    env' = compileArgs defs env
compileLet :: [(Name, CoreExpr)] -> GmCompiler -> (Int -> GmCode) -> GmCompiler
compileLet defs compileExp cleanStack exp env =
  compileLetDefs defs env ++ compileExp exp env' ++ cleanStack (length defs)
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
compileE (Let False defs exp) env = compileLet defs compileE (\n -> [Slide n]) exp env
compileE (Let True defs exp) env = compileLetrec compileC defs compileE (\n -> [Slide n]) exp env
compileE e@(Application (Application (Var op) e0) e1) env
  | op `elem` domain builtInDyadicNumOperator = compileB e env ++ [MakeInt]
  | op `elem` domain builtInDyadicBoolOperator = compileB e env ++ [MakeBool]
compileE e@(Application (Var "negate") _) env = compileB e env ++ [MakeInt]
compileE (Application (Application (Application (Var "if") e0) e1) e2) env =
  compileB e0 env ++ [Cond (compileE e1 env) (compileE e2 env)]
compileE (Application (Application (Var "par") e1) e2) env =
  compileC e2 env ++ [Push 0, Parallel] ++
  compileC e1 (argOffset 1 env) ++ [MakeApplication, Eval]
compileE (Case e alts) env = compileE e env ++ [CaseJump (compileAlts compileEAltBody alts env)]
compileE e env = compileC e env ++ [Eval]

-- alts 这里为什么没有清理栈上项的操作？在 compile 里了
compileAlts :: (Int -> GmCompiler) -> [Alter Name] -> GmEnvironment -> [(Int, GmCode)]
compileAlts compile alts env = [(tag, compile (length names) body (zip names [0..] ++ argOffset (length names) env)) | (tag, names, body) <- alts]

compileEAltBody :: Int -> GmCompiler
compileEAltBody offset exp env = Split offset : compileE exp env ++ [Slide offset]

compileB :: GmCompiler
compileB (Num n) env = [PushBasic n]
compileB (Let False defs exp) env = compileLet defs compileB (\n -> [Pop n]) exp env
compileB (Let True defs exp) env = compileLetrec compileC defs compileB (\n -> [Pop n]) exp env
-- caller will ensure op is dyadic operator
compileB (Application (Application (Var op) e0) e1) env = compileB e1 env ++ compileB e0 env ++ [lookup (builtInDyadicBoolOperator ++ builtInDyadicNumOperator) op (error "impossible")]
compileB (Application (Var "negate") e) env = compileB e env ++ [Neg]
compileB (Application (Application (Application (Var "if") e0) e1) e2) env =
  compileB e0 env ++ [Cond (compileB e1 env) (compileB e2 env)]
compileB e env = compileE e env ++ [Get]
