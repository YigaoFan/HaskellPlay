{-# LANGUAGE InstanceSigs #-}
module GMachine.Util where
import Heap (Addr, Heap, heapLookup)
import AST (Name)

type GmStack = [Addr]
type GmVStack = [Int]
type GmDumpItem = (GmCode, GmStack, GmVStack)
type GmDump = [GmDumpItem]
type GmCode = [Instruction]
type GmHeap = Heap Node
type GmGlobals = [(Name, Addr)]
type GmStats = Int
type GmEnvironment = [(Name, Int)]
type GmOutput = String

domain :: [(a, b)] -> [a]
domain list = [key | (key, _) <- list]

initialStat :: GmStats
initialStat = 0
incStatSteps s = s + 1
getStatSteps s = s
data GmState = GmState
  {
    output :: GmOutput,
    code :: GmCode,
    stack :: GmStack,
    dump :: GmDump,
    vStack :: GmVStack,
    heap :: GmHeap,
    globals :: GmGlobals,
    stats :: GmStats
  }

setOutput :: GmOutput -> GmState -> GmState
setOutput o s = s { output = o }
setCode :: GmCode -> GmState -> GmState
setCode c s = s { code = c }
setStack :: GmStack -> GmState -> GmState
setStack st s = s { stack = st }
setDump :: GmDump -> GmState -> GmState
setDump d s = s { dump = d }
setVStack :: GmVStack -> GmState -> GmState
setVStack v s = s { vStack = v }
setHeap :: GmHeap -> GmState -> GmState
setHeap h s = s { heap = h }
setGlobals :: GmGlobals -> GmState -> GmState
setGlobals g s = s { globals = g }
setStats :: GmStats -> GmState -> GmState
setStats sts s = s { stats = sts }

nodeOfTopStack :: GmState -> Node
nodeOfTopStack state = heapLookup (heap state) (head (stack state))

data Instruction =
  Unwind | PushGlobal Name |
  PushInt Int | Push Int |
  MakeApplication | Update Int |
  Pop Int | Alloc Int | Slide Int |
  Eval | Add | Sub | Mul | Div | Neg |
  Eq | Ne | Lt | Le | Gt | Ge | Cond GmCode GmCode |
  Pack Int Int | CaseJump [(Int, GmCode)] |
  Split Int | Print |
  PushBasic Int | MakeBool | MakeInt | Get |
  Return
  deriving Show

instance Eq Instruction where
  Unwind == Unwind = True
  PushGlobal a == PushGlobal b = a == b
  PushInt a == PushInt b = a == b
  Push a == Push b = a == b
  MakeApplication == MakeApplication = True
  Update a == Update b = a == b
  _ == _ = False

data Node = Num Int
  | String String
  | Application Addr Addr
  | Global Int GmCode
  | Indirect Addr
  | Uninit
  | Construct Int [Addr]
  deriving Show

instance Eq Node where
  (==) :: Node -> Node -> Bool
  Num a == Num b = a == b
  Application a b == Application c d = False
  Global a b == Global c d = False
  Indirect a == Indirect b = False
  Construct a b == Construct c d = False
  

