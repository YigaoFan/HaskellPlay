module GMachine.Printer where
import GMachine.Util (GmState (heap, globals, stats, stack), Instruction (Unwind, PushGlobal, Push, PushInt, MakeApplication, Slide), Node (Global, Num, Application), GmCode, getStatSteps)
import PrettyPrint (display, concat, str, Sequence (Newline, Append, Indent), interleave, num, showAddr, layn)
import Prelude hiding (concat)
import Heap (Addr, heapLookup)
import AST (Name)

showResults :: [GmState] -> [Char]
showResults states@(s : remain) =
  display (concat [
    str "SuperCombinator definitions", Newline,
    interleave Newline (map (showSuperCombinator s) (globals s)),
    Newline, Newline, str "State transitions", Newline, Newline,
    layn (map showState states),
    Newline, Newline,
    showStats (last states)
  ])

showSuperCombinator :: GmState -> (Name, Addr) -> Sequence
showSuperCombinator s (name, addr) =
  concat [
    str "Code for ", str name, Newline,
    showInstructions code, Newline, Newline
  ]
  where (Global _ code) = heapLookup (heap s) addr

showInstructions :: GmCode -> Sequence
showInstructions code =
  concat [
    str "   Code:{", Newline,
    Indent (interleave Newline (map showInstruction code)),
    str "}", Newline
  ]

showInstruction :: Instruction -> Sequence
showInstruction Unwind = str "Unwind"
showInstruction (PushGlobal f) = Append (str "PushGlobal ") (str f)
showInstruction (Push n) = Append (str "Push ") (num n)
showInstruction (PushInt n) = Append (str "PushInt ") (num n)
showInstruction MakeApplication = str "MakeApplication"
showInstruction (Slide n) = Append (str "Slide ") (num n)

showState :: GmState -> Sequence
showState state =
  concat []

showStack :: GmState -> Sequence
showStack state =
  concat [
    str "Stack:[",
    Indent (interleave Newline (map (showStackItem state) (reverse (stack state)))),
    str "]"
  ]
showStackItem :: GmState -> Addr -> Sequence
showStackItem state addr =
  concat [showAddr addr, str ": ", showNode state addr (heapLookup (heap state) addr)]
showNode :: GmState -> Addr -> Node -> Sequence
showNode state addr (Num n) = num n
showNode state addr (Global n g) = concat [str "Global ", str v]
  where v = head [n | (n, a) <- globals state, addr == a]
showNode state addr (Application a1 a2) =
  concat [str "Application ", showAddr a1, str " ", showAddr a2]

showStats :: GmState -> Sequence
showStats state =
  concat [str "Steps taken = ", num (getStatSteps (stats state))]