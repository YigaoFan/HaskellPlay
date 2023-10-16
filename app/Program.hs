module Program where

import Parser (parse)
import Compiler (compile)
import Evaluator (showResults, eval)
run = showResults . eval . compile . parse