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
import Program (run)

-- let scs = syntax $ lex
--           "f = 3;\n\
--           \g x y = let z = x in z;\n\
--           \h x = case (let y = x in y) of\n\
--           \<1> -> 2;\n\
--           \<2> -> 5 "
--           1
--     print (length scs)
-- 写 parser 测试
src0 = 
  "pair x y f = f x y;\n\
  \fst p = p left;\n\
  \snd p = p right;\n\
  \f x y = letrec\n\
  \        a = pair x b;\n\
  \        b = pair y a\n\
  \        in\n\
  \        fst (snd (snd (snd a)));\n\
  \main = f (id 3) 4"

src1 = "main = letrec f = f x in f" -- not return 应该是暂时不支持这种
src2 = "main = twice twice id 3" -- 想一下 indirect 这个的过程，较之上一版本(33)少了一步

src3 = "main = twice negate 3"

src4 = "main = negate (id 3)"

src5 = "main = 1 + 2 * 3 / 1"
main :: IO ()
main = do
  let r = run src5
  -- 去掉部分 indirect 后，现在成 35 了，还没对比 heap 变化
  putStr r

-- difference between putStr and print

    
