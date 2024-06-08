module Parser where

-- import Data.Bool (Bool)
-- import Data.String (String)
-- import GHC.Int (Int)
import Data.Char (isAlpha, isDigit)
import ParserCombinator (Parser, satisfy, apply, oneOrMoreWithSep, next4, zeroOrMore, alt, next3, next5, next, oneOrMore, empty, option)
import Data.Foldable (notElem)
import Data.Eq ((==))
import GHC.Base ( (||), (&&) )
import Text.Read (read)
import AST (CoreExpr, CoreProgram, CoreSuperCombinator, Expr (Var, Num, String, Application, Constructor, Let, Case, Lambda), makeSuperCombinator, Name, Alter)
import Lexer (Token, lex)
import Prelude hiding (exp, Left, Right, lex)
import Data.List (find, map, isPrefixOf, isSuffixOf)

keywords :: [String]
keywords = ["let", "letrc", "case", "in", "of", "Pack"]

-- refine this def TODO
var :: Parser String
var = satisfy (\t@(c : cs) -> notElem t keywords && (isAlpha c || '_' == c))

lit :: String -> Parser String
lit literal = satisfy (==literal)

-- 感觉分词那边可以把 token 类型存下来，parser 这边直接读来分别处理，解析这边比较依赖分词那边的结果，两边需要配合
num :: Parser Int
num = apply (satisfy (\(c : _) -> isDigit c)) (\s -> read s :: Int)

string :: Parser String
string = apply (satisfy (isPrefixOf "\"")) (removeEscChar . removeHeadTailQuote)

pack :: Parser CoreExpr
pack = next5 (\_ tag _ arity _ -> Constructor tag arity) (next const (lit "Pack") (lit "{")) num (lit ",") num (lit "}")

expWithParen :: Parser CoreExpr
expWithParen = next3 (\_ e _ -> e) (lit "(") exp (lit ")")

removeHeadTailQuote :: String -> String
removeHeadTailQuote ('"' : s) | "\"" `isSuffixOf` s = init s
removeHeadTailQuote _ = error "wrong format string"
removeEscChar :: String -> String
removeEscChar (c : c1 : s) | c == '\\' = c1 : s
removeEscChar (c : s) = c : removeEscChar s
removeEscChar [] = []
atomicExp :: Parser CoreExpr
atomicExp = alt [apply var Var, apply num Num, apply string String, pack, expWithParen]

-- 我感觉书上说的这种变换，虽然语义没变，因为 atomic exp 和 exp 互相引用的关系（是基本等同），但是语法变了
application :: Parser CoreExpr
application = apply (oneOrMore atomicExp) (\(ae0 : aes) ->
  if null aes
    then ae0
    else foldl Application (Application ae0 (head aes)) (tail aes))

data PartialExpr = NoOp | FoundOp Name CoreExpr
asmOp :: CoreExpr -> PartialExpr -> CoreExpr
asmOp ce NoOp = ce
asmOp ce0 (FoundOp n ce1) = Application (Application (Var n) ce0) ce1
exp1 :: Parser CoreExpr
exp1 = next asmOp exp2 exp1c
exp1c :: Parser PartialExpr
exp1c = alt [next FoundOp (lit "|") exp1, empty NoOp]
exp2 :: Parser CoreExpr
exp2 = next asmOp exp3 exp2c
exp2c = alt [next FoundOp (lit "&") exp2, empty NoOp]
exp3 :: Parser CoreExpr
exp3 = next asmOp exp4 exp3c
exp3c = alt (empty NoOp : map (\op -> next FoundOp (lit op) exp4) ["==", "~=", ">", ">=", "<", "<="])
exp4 :: Parser CoreExpr
exp4 = next asmOp exp5 exp4c
exp4c = alt [empty NoOp, next FoundOp (lit "+") exp4, next FoundOp (lit "-") exp5]
exp5 :: Parser CoreExpr
exp5 = next asmOp exp6 exp5c
exp5c = alt [empty NoOp, next FoundOp (lit "*") exp5, next FoundOp (lit "/") exp6]

exp6 = application

def :: Parser (Name, CoreExpr)
def = next3 (\v _ e -> (v, e)) var (lit "=") exp
defs :: Parser [(Name, CoreExpr)]
defs = next3 (\_ x _ -> x) (option (lit "\n")) (oneOrMoreWithSep def (lit "\n")) (option (lit "\n"))
localDef :: Parser CoreExpr
localDef = next4 (\_ ds _ e -> Let False ds e) (lit "let") defs (lit "in") exp
localRecurDef :: Parser CoreExpr
localRecurDef = next4 (\_ ds _ e -> Let True ds e) (lit "letrec") defs (lit "in") exp

caseAlt :: Parser (Alter Name)
caseAlt = next4 (\n vs _ e -> (n, vs, e))
  (next3 (\_ n _ -> n) (lit "<") num (lit ">"))
  (zeroOrMore var)
  (lit "->")
  exp
caseExp :: Parser CoreExpr
caseExp = next5 (\_ e _ _ alts -> Case e alts) (lit "case") exp (lit "of") (lit "\n") (oneOrMoreWithSep caseAlt (lit "\n"))

lambda :: Parser CoreExpr
lambda = next4 (\_ vs _ e -> Lambda vs e) (lit "\\") (oneOrMore var) (lit ".") exp
exp :: Parser CoreExpr
exp = alt [
    localDef,
    localRecurDef,
    caseExp,
    lambda,
    atomicExp,
    exp1
  ]

coreSuperCombinator :: Parser CoreSuperCombinator
coreSuperCombinator = next4 (\v as _ e -> makeSuperCombinator v as e) var (zeroOrMore var) (lit "=") exp
program :: Parser CoreProgram
program = next3 (\_ x _ -> x) (zeroOrMore (lit "\n")) (oneOrMore coreSuperCombinator) (zeroOrMore (lit "\n"))

syntax :: [Token] -> CoreProgram
syntax = takeFirstFullParse . program

takeFirstFullParse ((ast, []) : _) = ast
takeFirstFullParse (_ : others) = takeFirstFullParse others
takeFirstFullParse r = error ("Syntax error: result length " ++ show (length r))

allSyntax :: [Token] -> [CoreProgram]
allSyntax = map fst . program

parse :: [Char] -> CoreProgram
parse s = syntax (lex s 1)

