module GMachine.Printer where
import GMachine.Util (GmState (heap, globals, stats, stack, code, dump), Instruction (..), Node (Global, Num, Application, Indirect), GmCode, getStatSteps, GmDumpItem, GmStack, GmHeap)
import PrettyPrint (display, concat, str, Sequence (Newline, Append, Indent, Nil), interleave, num, showAddr, layn)
import Prelude hiding (concat)
import Heap (Addr, heapLookup)
import AST (Name)

-- 显示的函数不要管两头的换行，只管内部的换行

showResults :: [GmState] -> [Char]
showResults states@(s : remain) =
  display (concat [
    str "SuperCombinator definitions", Newline,
    interleave Newline (map (showSuperCombinator s) (globals s)), Newline, Newline,
    str "State transitions", Newline,
    layn (map showState states), Newline,
    showHeap (last states) (heap (last states)), Newline,
    showStats (last states)
  ])

showSuperCombinator :: GmState -> (Name, Addr) -> Sequence
showSuperCombinator s (name, addr) =
  concat [
    str "Code for ", str name, Newline,
    showInstructions code
  ]
  where (Global _ code) = heapLookup (heap s) addr

showInstructions :: GmCode -> Sequence
showInstructions code =
  concat [
    str "Code:{", Newline,
    Indent (appendNewLineIfNotNull (map showInstruction code)),
    str "}"
  ]

showInstruction :: Instruction -> Sequence
showInstruction Unwind = str "Unwind"
showInstruction (PushGlobal f) = Append (str "PushGlobal ") (str f)
showInstruction (Push n) = Append (str "Push ") (num n)
showInstruction (PushInt n) = Append (str "PushInt ") (num n)
showInstruction MakeApplication = str "MakeApplication"
showInstruction (Update a) = Append (str "Update ") (num a)
showInstruction (Pop a) = Append (str "Pop ") (num a)
showInstruction (Slide a) = Append (str "Slide ") (num a)
showInstruction (Alloc a) = Append (str "Alloc ") (num a)
showInstruction Eval = str "Eval"
showInstruction Add = str "Add"
showInstruction Sub = str "Sub"
showInstruction Mul = str "Mul"
showInstruction Div = str "Div"
showInstruction Neg = str "Neg"
showInstruction Eq = str "Eq"
showInstruction Ne = str "Ne"
showInstruction Lt = str "Lt"
showInstruction Le = str "Le"
showInstruction Gt = str "Gt"
showInstruction Ge = str "Ge"
showInstruction (Cond code1 code2) = Append (showInstructions code1) (showInstructions code2)

showState :: GmState -> Sequence
showState state =
  concat [
    showStack state, Newline,
    showDump state, Newline,
    showInstructions (code state)
  ]

showStack :: GmState -> Sequence
showStack state =
  concat [
    str "Stack:[", Newline,
    Indent (appendNewLineIfNotNull (map (showStackItem state) (reverse (stack state)))),
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
showNode state addr (Indirect a) = concat [str "Indirect ", num a]

showDump :: GmState -> Sequence
showDump state =
  concat [
    str "Dump:[", Newline,
    Indent (appendNewLineIfNotNull (map showDumpItem (reverse (dump state)))),
    str "]"
  ]
showDumpItem :: GmDumpItem -> Sequence
showDumpItem (code, stack) =
  concat [
    str "<",
    shortShowInstructions 3 code, str ", ",
    shortShowStack stack, 
    str ">"
  ]
shortShowInstructions :: Int -> GmCode -> Sequence
shortShowInstructions num code =
  concat [
    str "{", interleave (str "; ") dotCodes, str "}"
  ]
  where
    codes = map showInstruction (take num code)
    dotCodes | length code > num = codes ++ [str "..."]
             | otherwise = codes
shortShowStack :: GmStack -> Sequence
shortShowStack stack =
  concat [
    str "[",
    interleave (str ", ") (map showAddr stack),
    str "]"
  ]

showStats :: GmState -> Sequence
showStats state =
  concat [str "Steps taken = ", num (getStatSteps (stats state))]

showHeap :: GmState -> GmHeap -> Sequence
showHeap state (size, free, addrObjs) =
  concat
    [
      str "count: ", num size, Newline,
      str "{", Newline,
      Indent (interleave Newline (map (\(addr, obj) -> concat [showAddr addr, str " : ", showNode state addr obj]) addrObjs)), Newline,
      str "}"
    ]

appendNewLineIfNotNull :: [Sequence] -> Sequence
appendNewLineIfNotNull lines =
      Append (interleave Newline lines)
        (if not (null lines)
          then Newline
          else Nil)