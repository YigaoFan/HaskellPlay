module Parser (lit) where

import Data.Bool (Bool)
import Data.String (String)
import GHC.Int (Int)
import qualified Data.Char as Char
import Data.Char (isDigit)
import ParserCombinator (Parser, satisfy, apply)
import Data.Foldable (notElem)
import Data.Eq ((==))
import GHC.Base ( (||), (&&) )
import Text.Read (read)

keywords :: [String]
keywords = ["let", "letrc", "case", "in", "of", "Pack"]

var :: Parser String
var = satisfy (\t@(c : cs) -> notElem t keywords && (Char.isAlpha c || '_' == c))

lit :: String -> Parser String
lit literal = satisfy (==literal)

-- 感觉分词那边可以把 token 类型存下来，parser 这边直接读来分别处理，解析这边比较依赖分词那边的结果，两边需要配合
num :: Parser Int
num = apply (satisfy (\(c : _) -> isDigit c)) (\s -> read s :: Int)


