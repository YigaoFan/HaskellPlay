module GMachine.Program where
import GMachine.Evaluator (eval)
import GMachine.Compiler (compile)
import Parser (parse)

run :: [Char] -> [Char]
run = eval . compile . parse
-- pattern is what
