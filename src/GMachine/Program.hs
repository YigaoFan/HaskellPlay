module GMachine.Program where

import GMachine.Evaluator (eval)
import GMachine.Compiler (compile, compileSuperCombinator)
import Parser (parse)
import GMachine.Printer (showResults)

run :: [Char] -> [Char]
run = showResults . eval . compile . parse

exe = eval . compile . parse
