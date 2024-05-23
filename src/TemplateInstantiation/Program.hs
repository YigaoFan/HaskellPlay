module TemplateInstantiation.Program where

import Parser (parse)
import TemplateInstantiation.Compiler (TiHeap, TiState, compile)
import TemplateInstantiation.Evaluator (eval, showResults)
import Heap (Addr)
run = showResults . eval . compile . parse

resultOf :: [TiState] -> (Addr, TiHeap)
resultOf states = (head stack, heap) --when encounter stop, it's empty!
  where (_, stack, _, heap, _, _) = last states
-- | execute code, return the top of stack and heap. This is for convenient testing
exe = resultOf . eval . compile . parse
