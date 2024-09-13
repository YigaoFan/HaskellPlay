module ParallelGMachine.Program where

import ParallelGMachine.Printer (showResults)
import ParallelGMachine.Evaluator (eval)
import ParallelGMachine.Compiler (compile)
import Parser (parse)

run :: [Char] -> [Char]
run = showResults . eval . compile . parse