module TIM.Printer where

import TIM.Util (TimState (..), TimHeap, FramePtr (FrameAddr, FrameNull, FrameInt), TimStack, TimValueStack, TimDump, Closure, getStepsFromStats, Instruction (..), TimAddrMode (..), TimCode, maxStackDepth, ValueAddrMode (..), getClosure)
import PrettyPrint (display, concat, Sequence (Newline, Indent, Append, Nil), layn, str, interleave, num, laynList, displaySeqs, laynAsSeqs, interleaveAsSeqs)
import Prelude hiding (concat)
import Heap (heapLookup, heapSize)
import AST (Name)
import Debug.Trace (trace)
import Data.List (mapAccumL)

-- 每个函数只负责内部的换行，除了 showResults 函数
data HowMuchToPrint = Full | Terse | None
terseNum = 3

showResults :: [TimState] -> [Char]
showResults states =
  display (concat ([
    str "Output:", Newline] ++
    map str os ++ [Newline,
    showState lastState, Newline,
    showStats lastState, Newline
  ]))
  where
    lastState = last states
    (_, os) = mapAccumL (\a b -> let la = length a in if length b > la then (b, drop la b) else (b, "")) "" (map output states)

showFullResults :: [TimState] -> [Char]
showFullResults states =
  displaySeqs (
    str "SuperCombinator definitions" : Newline :
    showSuperCombinatorDefs (head states) : Newline : Newline :
    str "State transitions" : Newline :
    laynAsSeqs (map showState states) ++ [Newline, Newline,
    showStats (last states), Newline,
    str "Output:", showOutput (last states)])

showSuperCombinatorDefs :: TimState -> Sequence
showSuperCombinatorDefs state =
  interleave Newline (map (\(n, i) -> showSuperCombinator (n, fst $ getClosure (heap state) f i)) maps)
  where
    (f, maps) = codeStore state

showState :: TimState -> Sequence
showState state =
  concat [
    str "Code: ", showInstructions Terse (code state), Newline,
    showFrame (heap state) (framePtr state), Newline,
    showDataFrame (heap state) (dataFramePtr state), Newline,
    showStack (stack state), Newline,
    showValueStack (valueStack state), Newline,
    showDump (dump state)
  ]
showFrame :: TimHeap -> FramePtr -> Sequence
showFrame heap FrameNull = str "Null frame ptr"
showFrame heap (FrameAddr addr) =
  let cs = heapLookup heap addr in
    concat [
      str "Frame: ", num (length cs), str "item(s) <",
      Indent (interleave Newline (map showClosure cs)),
      str ">"
    ]
showFrame heap (FrameInt n) =
  concat [
    str "Frame ptr (int): ", num n
  ]

showDataFrame :: TimHeap -> FramePtr -> Sequence
showDataFrame heap FrameNull = str "Null data frame ptr"
showDataFrame heap (FrameAddr addr) =
  let cs = heapLookup heap addr
   in concat
        [ str "DataFrame: ",
          num (length cs),
          str "item(s) <",
          Indent (interleave Newline (map showClosure cs)),
          str ">"
        ]
showDataFrame heap (FrameInt n) =
  concat
    [ str "DataFrame ptr (int): ",
      num n
    ]

showStack :: TimStack -> Sequence
showStack stack =
  concat [
    str "Arg stack: [",
    Indent (interleave Newline (map showClosure stack)),
    str "]"
  ]

showValueStack :: TimValueStack -> Sequence
showValueStack valueStack = concat [
  str "VStack: [ ", interleave (str ", ") (map num valueStack), str " ]"
    ]

showDumpItem :: (FramePtr, Int, TimStack) -> Sequence
showDumpItem (framePtr, x, stk) = concat (
  str "(" : showFramePtr framePtr : str "," : num x : str ", " : str "[" : interleaveAsSeqs (Append (str ",") Newline) (map showClosure stk) ++ [str "]", str ")"])

showDump :: TimDump -> Sequence
showDump dump = concat (str "Dump: [" : map showDumpItem dump ++ [str "]"])

showClosure :: Closure -> Sequence
showClosure (i, f) =
  concat [
    str "(", showInstructions Terse i, str ", ", showFramePtr f, str ")"
  ]

showFramePtr :: FramePtr -> Sequence
showFramePtr FrameNull = str "null"
showFramePtr (FrameAddr a) = str (show a)
showFramePtr (FrameInt n) = Append (str "int ") (num n)

showStats :: TimState -> Sequence
showStats state =
  concat [
    str "Steps taken = ", num (getStepsFromStats (stats state)), Newline,
    str "Num of frames allocated = ", num (heapSize (heap state)), Newline,
    str "Max stack depth = ", num (maxStackDepth (stats state))
  ]

showOutput :: TimState -> Sequence
showOutput state = str (output state)

showInstructions :: HowMuchToPrint -> [Instruction] -> Sequence
showInstructions None _ = str "{..}"
showInstructions Terse is =
  concat [ str "{ ", interleave (str ", ") body, str " }"]
  where
    instrs = map (showInstruction None) is
    body | length is <= terseNum = instrs
         | otherwise           = take terseNum instrs ++ [str ".."]
showInstructions Full is =
  concat [
    str "{ ", Newline,
    Indent (interleave (Append (str ",") Newline) shownInstructions), Newline,
    str "}"
  ]
  where shownInstructions = map (showInstruction Full) is

showInstruction :: HowMuchToPrint -> Instruction -> Sequence
showInstruction detail (Take cap n) = concat [str "Take ", num cap, str " ", num n]
showInstruction detail (Enter addrMode) = str "Enter " `Append` showArg detail addrMode
showInstruction detail (Push addrMode) = str "Push " `Append` showArg detail addrMode
showInstruction detail (PushV addrMode) = str "PushV " `Append` showValueArg detail addrMode
showInstruction detail Return = str "Return"
showInstruction detail (Op op) = str "Op " `Append` str (show op)
showInstruction detail (Cond code1 code2) = concat [
  str "Cond", Newline,
  Indent (interleave Newline (map (showInstruction detail) code1)), Newline,
  Indent (interleave Newline (map (showInstruction detail) code2))
  ]
showInstruction detail (Move n addr) = concat [str "Move ", num n, str " ", showArg detail addr]
showInstruction detail (PushMarker n) = concat [str "PushMarker ", num n]
showInstruction detail (UpdateMarkers n) = concat [str "UpdateMarkers ", num n]
showInstruction detail (Switch alts) = concat [
  str "Switch ", Newline,
  Indent (interleave Newline (map (\(t, is) -> concat [num t, str " -> ", showInstructions detail is]) alts))
  ]
showInstruction detail (ReturnConstructor tag) = concat [
  str "ReturnConstructor ",
  num tag
  ]
showInstruction detail Print = str "Print"
showArg :: HowMuchToPrint -> TimAddrMode -> Sequence
showArg detail (Arg n) = str "Arg " `Append` num n
showArg detail (Code is) = str "Code " `Append` showInstructions detail is
showArg detail (Label name k) = concat [str "Label ", str name, str " ", num k]
showArg detail (IntConst n) = str "IntConst " `Append` num n
showArg detail (Data n) = str "Data " `Append` num n

showValueArg :: HowMuchToPrint -> ValueAddrMode -> Sequence
showValueArg detail FramePtr = str "FramePtr"
showValueArg detail (IntValueConst n) = str "IntValueConst " `Append` num n

showSuperCombinator :: (Name, TimCode) -> Sequence
showSuperCombinator (name, code) =
  concat [
    str "Code for ", str name, str ":", Newline,
    showInstructions Full code
  ]
