module Main where

import AST (Expr (Var))
import GHC.IO (IO)
-- import qualified PrettyPrint (print)
import PrettyPrint (makeMultiApplication, display, makeSentence, makeCalculate)
import System.IO (putStrLn)
import Prelude hiding (print, lex)
import Lexer (lex)
import Parser (syntax, allSyntax, parse)
import Prelude (print)
-- import TemplateInstantiation.Program (run)
-- import GMachine.Program (run)
import GMachine.Compiler (compileSuperCombinator)
import TIM.Program (run, fullRun)

src0 =
  "pair x y f = f x y;\n\
  \fst p = p left;\n\
  \snd p = p right;\n\
  \f x y = letrec\n\
  \        a = pair x b;\n\
  \        b = pair y a\n\
  \        in\n\
  \        fst (snd (snd (snd a)));\n\
  \main = f (id 3) 4;"

src1 = "main = letrec f = f x in f;" -- not return 应该是暂时不支持这种
src2 = "main = twice twice id 3;" -- 想一下 indirect 这个的过程，较之上一版本(33)少了一步

src3 = "main = twice negate 3;"

src4 = "main = negate (id 3);"

src5 = "main = 1 + 2 * 3 / 1;"

src6 = "fac n = if (n == 0) 1 (n * fac (n-1));\n\
  \main = fac 3;"

-- TODO 试一下嵌套 if 和 fst
src7 =
  "MakePair = Pack{1, 2};\n\
  \fst p = casePair p left;\n\
  \snd p = casePair p right;\n\
  \Nil = Pack{1, 0};\n\
  \Cons = Pack{2, 2};\n\
  \main = snd (fst (MakePair (MakePair 1 (MakePair 2 3)) 4));"

src8 =
  "length xs = caseList xs 0 len;\n\
  \len x xs = 1 + length xs; \n\
  \Nil = Pack{1, 0};\n\
  \Cons = Pack{2, 2};\n\
  \main = length (Cons 1 (Cons 2 Nil));"

src9 =
  -- 测试 lambda 支持 TODO 暂时不做，因为 lambda 需要 capture env 和 heap，可能需要大变动
  "length xs = caseList xs 0 (\\i is. 1 + length is);\n\
  \Nil = Pack{1, 0};\n\
  \Cons = Pack{2, 2};\n\
  \main = length (Cons 1 (Cons 2 Nil));"
-- abort 加个参数支持
src10 = --lambda version
  "length xs = caseList xs 0 len;\n\
  \len x xs = 1 + length xs;\n\
  \head xs = caseList xs abort (\\i is. i);\n\
  \tail xs = caseList xs abort (\\i is. is);\n\
  \Nil = Pack{1, 0};\n\
  \Cons = Pack{2, 2};\n\
  \main = head (Cons 1 (Cons 2 Nil));"

src11 = "main = abort \"hello abort\";"

src12 = -- exercise 2.25 缺点是语法不一致
  -- without lambda version
  "length xs = caseList xs 0 len;\n\
  \len x xs = 1 + length xs;\n\
  \head xs = caseList xs (abort \"empty list\") headHandleNonEmtpy;\n\
  \headHandleNonEmtpy i is = i;\n\
  \tail xs = caseList xs (abort \"empty list\") tailHandleNonEmtpy;\n\
  \tailHandleNonEmtpy i is = is;\n\
  \Nil = Pack{1, 0};\n\
  \Cons = Pack{2, 2};\n\
  \main = head (tail (Cons 1 (Cons 2 Nil)));"

-- add to prelude
src13 = "main = head (Cons 1 Nil);"

src14 =
  "returnList = Cons 1 (Cons 2 (Cons 3 Nil));\n\
  \main = returnList;"

src15 =
  "apply a = a 1\n\
  \add a b = a + b\n\
  \main = apply (add 1)"

src16 = "main = 3 + 4 * 5"

src17 =
  "returnFunc a b fa fb = if (a > b) fa fb\n\
  \add a b = a + b\n\
  \main = (returnFunc 1 2 (add 1) (add 2)) 2"

src18 = "main = if (1 /= 5) 1 (5 + 2)" -- 现在语法中没分号了！
-- 为什么加括号后好多了

src19 = "main = Pack{1, 2} 4 2"

src20 = 
  "fac n = if (n == 0) 1 (n * fac (n - 1))\n\
   \main = fac 20"

src21 =
  "f x = Pack{2, 2} (case x of\n <1> -> 1\n <2> -> 2) Pack{1, 0}\n\
  \Nil = Pack{1, 0}\n\
  \Cons = Pack{2, 2}\n\
  \map f xs = case xs of\n\
    \<1> -> Nil\n\
    \<2> x remain -> Cons (f x) (map f xs)\n\
  \prefix p xs = map (Pack{2, 2} p) xs\n\
  \main = 1"

src22 =
  "main = (letrec a = 1 in a + 1) + (let b = 2 in b - 1)"

src23 = "main = negate 3"

src24 = "main = s left left 4"

src25 =
  "id = s left left\n\
  \id1 = id id\n\
  \main = id1 4"

-- TODO 实现下 case
main :: IO ()
main = do
  let r = run src25
  -- 去掉部分 indirect 后，现在成 35 了，还没对比 heap 变化
  putStr r

-- main = do
--   let (_, _, c) = head (map compileSuperCombinator (parse "Y f = letrec \nx = f x\n in x"))
--   putStr (show c)

-- difference between putStr and print


