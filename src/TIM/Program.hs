module TIM.Program where

import TIM.Printer (showFullResults, showResults)
import TIM.Evaluator (eval)
import TIM.Compiler (compile)
import Parser (parse)

run :: [Char] -> [Char]
run = showResults . eval . compile . parse

fullRun :: [Char] -> [Char]
fullRun = showFullResults . eval . compile . parse