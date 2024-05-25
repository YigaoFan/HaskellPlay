module GMachine.Util where
import Heap (Addr, Heap)
import AST (Name)

type GmStack = [Addr]
type GmCode = [Instruction]
type GmHeap = Heap Node
type GmGlobals = [(Name, Addr)]
type GmStats = Int
type GmEnvironment = [(Name, Int)]

domain :: [(a, b)] -> [a]
domain list = [key | (key, _) <- list]

initialStat :: GmStats
initialStat = 0
incStatSteps s = s + 1
getStatSteps s = s
data GmState = GmState
  {
    code :: GmCode,
    stack :: GmStack,
    heap :: GmHeap,
    globals :: GmGlobals,
    stats :: GmStats
  }

setCode :: GmCode -> GmState -> GmState
setCode c s = s { code = c }
setStack :: GmStack -> GmState -> GmState
setStack st s = s { stack = st }
setHeap :: GmHeap -> GmState -> GmState
setHeap h s = s { heap = h }
setGlobals :: GmGlobals -> GmState -> GmState
setGlobals g s = s { globals = g }
setStats :: GmStats -> GmState -> GmState
setStats sts s = s {stats = sts}

data Instruction =
  Unwind | PushGlobal Name |
  PushInt Int | Push Int |
  MakeApplication | Slide Int
  deriving Show

instance Eq Instruction where
  Unwind == Unwind = True
  PushGlobal a == PushGlobal b = a == b
  PushInt a == PushInt b = a == b
  Push a == Push b = a == b
  MakeApplication == MakeApplication = True
  Slide a == Slide b = a == b
  _ == _ = False

data Node = Num Int
  | Application Addr Addr
  | Global Int GmCode

