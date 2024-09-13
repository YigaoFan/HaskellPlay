module GMachine.Printer where
import GMachine.Util
    ( GmState(heap, globals, stats, stack, code, dump, output, vStack),
      Instruction(..),
      Node(..),
      GmCode,
      getStatSteps,
      GmDumpItem,
      GmStack,
      GmHeap,
      Node(String), GmVStack, GmDump, GmGlobals, GmOutput )
import PrettyPrint (display, concat, str, Sequence (Newline, Append, Indent, Nil), interleave, num, showAddr, layn, appendNewLineIfNotNull, showItems)
import Prelude hiding (concat)
import Heap (Addr, heapLookup)
import AST (Name)

-- 显示的函数不要管两头的换行，只管内部的换行

showResults :: [GmState] -> [Char]
showResults states@(s : remain) =
  display (concat [
    str "SuperCombinator definitions", Newline,
    interleave Newline (map (showSuperCombinator (heap s)) (globals s)), Newline, Newline,
    str "State transitions", Newline,
    layn (map showState states), Newline, -- 这里能不能改成一个一个显示出来，否则由于惰性求值一个出错，所有都显示不出来了。除了 reverse，我想不到其他影响惰性的地方了。
    showHeap (last states) (heap (last states)), Newline,
    showStats (last states), Newline,
    showOutput (output (last states)), Newline
  ])

showSuperCombinator :: GmHeap -> (Name, Addr) -> Sequence
showSuperCombinator heap (name, addr) =
  concat [
    str "Code for ", str name, Newline,
    showInstructions code
  ]
  where (Global _ code) = heapLookup heap addr

showInstructions :: GmCode -> Sequence
showInstructions code =
  concat [
    str "Code: {", Newline,
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
showInstruction (Cond code1 code2) = concat [
  str "Cond", Newline,
  Indent (interleave Newline (map showInstruction code1)), Newline,
  Indent (interleave Newline (map showInstruction code2))
  ]
showInstruction (Pack a b) = concat [str "Pack ", num a, str ", ", num b]
showInstruction (CaseJump xs) = concat [str "CaseJump", Newline, Indent (interleave Newline (map (\(i, code) -> concat [str "(", num i, str " -> ", interleave (str ", ") (map showInstruction code), str ")"]) xs))]
showInstruction Print = str "Print"
showInstruction (Split a) = concat [str "Split ", num a]
showInstruction (PushBasic n) = Append (str "PushBasic ") (num n)
showInstruction MakeBool = str "MakeBool"
showInstruction MakeInt = str "MakeInt"
showInstruction Get = str "Get"
showInstruction Return = str "Return"
showInstruction Parallel = str "Parallel"

showState :: GmState -> Sequence
showState state =
  concat [
    showStack (stack state) (globals state) (heap state), Newline,
    showDump (dump state), Newline,
    showVStack (vStack state), Newline,
    showInstructions (code state), Newline,
    showOutput (output state)
  ]

showOutput :: GmOutput -> Sequence
showOutput output =
  concat [str "Output:\"", str (reverse output), str "\""]
showStack :: GmStack -> GmGlobals -> GmHeap -> Sequence
showStack stack globals heap =
  concat [
    str "Stack:[", Newline,
    Indent (appendNewLineIfNotNull (map (showStackItem globals heap) (reverse stack))),
    str "]"
  ]
showStackItem :: GmGlobals -> GmHeap -> Addr -> Sequence
showStackItem globals heap addr =
  concat [showAddr addr, str ": ", showNode globals addr (heapLookup heap addr)]
showNode :: GmGlobals -> Addr -> Node -> Sequence
showNode globals addr Uninit = str "Uninit"
showNode globals addr (Num n) = num n
showNode globals addr (String s) = str (show s)
showNode globals addr (Global n g) = concat [str "Global ", str v]
  where v = head [n | (n, a) <- globals, addr == a]
showNode globals addr (Application a1 a2) =
  concat [str "Application ", showAddr a1, str " ", showAddr a2]
showNode globals addr (Indirect a) = concat [str "Indirect ", num a]
showNode globals addr (Construct t coms) = concat [str "Construct ", num t, str " [",
  interleave (str ", ") (map showAddr coms), str "]"
  ]

showDump :: GmDump -> Sequence
showDump dump =
  concat [
    str "Dump:[", Newline,
    Indent (appendNewLineIfNotNull (map showDumpItem (reverse dump))),
    str "]"
  ]
showDumpItem :: GmDumpItem -> Sequence
showDumpItem (code, stack, v) =
  concat [
    str "<",
    shortShowInstructions 3 code, str ", ",
    shortShowStack stack,
    shortShowVStack v,
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
shortShowVStack :: GmVStack -> Sequence
shortShowVStack = showItems num

showVStack :: GmVStack -> Sequence
showVStack vStack =
  concat [
    str "VStack: ",
    showItems num vStack
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
      Indent (interleave Newline (map (\(addr, obj) -> concat [showAddr addr, str " : ", showNode (globals state) addr obj]) addrObjs)), Newline,
      str "}"
    ]