module TIM.Printer where

import TIM.Util (TimState (..), TimHeap, FramePtr (FrameAddr, FrameNull, FrameInt), TimStack, TimValueStack, TimDump, Closure, getStepsFromStats, Instruction (..), TimAddrMode (..), TimCode)
import PrettyPrint (display, concat, Sequence (Newline, Indent, Append, Nil), layn, str, interleave, num)
import Prelude hiding (concat)
import Heap (heapLookup, heapSize)
import AST (Name)

-- 每个函数只负责内部的换行，除了 showResults 函数
data HowMuchToPrint = Full | Terse | None
terseNum = 3

showResults :: [TimState] -> [Char]
showResults states =
  display (concat [
    showState lastState, Newline,
    showStats lastState, Newline
  ])
  where lastState = last states

showFullResults :: [TimState] -> [Char]
showFullResults states =
  display (concat [
    str "SuperCombinator definitions", Newline,
    showSuperCombinatorDefs (head states), Newline, Newline,
    str "State transitions", Newline,
    layn (map showState states), Newline, Newline,
    showStats (last states)
  ])

showSuperCombinatorDefs :: TimState -> Sequence
showSuperCombinatorDefs state =
  interleave Newline (map showSuperCombinator (codeStore state))

showState :: TimState -> Sequence
showState state =
  concat [
    str "Code: ", showInstructions Terse (code state), Newline,
    showFrame (heap state) (framePtr state), Newline,
    showStack (stack state), Newline,
    showValueStack (valueStack state),
    showDump (dump state)
  ]

showFrame :: TimHeap -> FramePtr -> Sequence
showFrame heap FrameNull = str "Null frame ptr"
showFrame heap (FrameAddr addr) =
  concat [
    str "Frame: <",
    Indent (interleave Newline (map showClosure (heapLookup heap addr))),
    str ">"
  ]
showFrame heap (FrameInt n) =
  concat [
    str "Frame ptr (int): ", num n
  ]

showStack :: TimStack -> Sequence
showStack stack =
  concat [
    str "Arg stack: [",
    Indent (interleave Newline (map showClosure stack)),
    str "]"
  ]

showValueStack :: TimValueStack -> Sequence
showValueStack valueStack = Nil

showDump :: TimDump -> Sequence
showDump dump = Nil

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
    str "Num of frames allocated = ", num (heapSize (heap state))
  ]

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
showInstruction detail (Take n) = str "Take " `Append` num n
showInstruction detail (Enter addrMode) = str "Enter " `Append` showArg detail addrMode
showInstruction detail (Push addrMode) = str "Push " `Append` showArg detail addrMode

showArg :: HowMuchToPrint -> TimAddrMode -> Sequence
showArg detail (Arg n) = str "Arg " `Append` num n
showArg detail (Code is) = str "Code " `Append` showInstructions detail is
showArg detail (Label name) = str "Label " `Append` str name
showArg detail (IntConst n) = str "IntConst " `Append` num n

showSuperCombinator :: (Name, TimCode) -> Sequence
showSuperCombinator (name, code) =
  concat [
    str "Code for ", str name, str ":", Newline,
    showInstructions Full code
  ]