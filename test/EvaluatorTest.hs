module EvaluatorTest where

import TemplateInstantiation.Program (exe)
import Test.HUnit
import TemplateInstantiation.Compiler (Node (Num))
import Heap (heapLookup)

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

src6 =
  "fac n = if (n == 0) 1 (n * fac (n-1));\n\
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
src10 =
  -- lambda version
  "length xs = caseList xs 0 len;\n\
  \len x xs = 1 + length xs;\n\
  \head xs = caseList xs abort (\\i is. i);\n\
  \tail xs = caseList xs abort (\\i is. is);\n\
  \Nil = Pack{1, 0};\n\
  \Cons = Pack{2, 2};\n\
  \main = head (Cons 1 (Cons 2 Nil));"

src11 = "main = abort \"hello abort\";"

src12 =
  -- exercise 2.25 缺点是语法不一致
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
src13 = "main = (Cons 1 Nil);"

src14 =
  "returnList = Cons 1 (Cons 2 (Cons 3 Nil));\n\
  \main = returnList;"

testEvaluator0 = TestCase (assertEqual "list func" (Num 2) (do 
  let (a, h) = exe src14
  heapLookup h a
  ))

tests = TestList [ 
  TestLabel "test evaluator" testEvaluator0
  ]