module Program where

import Parser (parse)
import Compiler (compile, TiState, TiHeap)
import Evaluator (showResults, eval)
import Heap (Addr)
run = showResults . eval . compile . parse

resultOf :: [TiState] -> (Addr, TiHeap)
resultOf states = (head stack, heap) --when encounter stop, it's empty!
  where (_, stack, _, heap, _, _) = last states
-- | execute code, return the top of stack and heap. This is for convenient testing
exe = resultOf . eval . compile . parse
