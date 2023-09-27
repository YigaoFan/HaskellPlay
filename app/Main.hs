module Main where

import AST (Expr (Var))
import GHC.IO (IO)
-- import qualified PrettyPrint (print)
import PrettyPrint (makeMultiApplication, display, makeSentence, makeCalculate)
import System.IO (putStrLn)
import Prelude hiding (print, lex)
import Lexer (lex)
import Parser (syntax, allSyntax)
import Prelude (print)

-- 写 parser 测试
main :: IO ()
main = do
    let scs = syntax $ lex
          "f = 3;\n\
          \g x y = let z = x in z;\n\
          \h x = case (let y = x in y) of\n\
          \<1> -> 2;\n\
          \<2> -> 5 "
          1
    print (length scs)
    -- let sys = allSyntax $ lex
    --       "f x y = case x of \n\
    --       \        <1> -> case y of\n\
    --       \               <1> -> 1;\n\
    --       \        <2> -> 5 "
    --       1
    -- print sys
