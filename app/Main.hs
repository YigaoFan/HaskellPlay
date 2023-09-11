module Main where

import AST (Expr (Var))
import GHC.IO (IO)
import PrettyPrint (makeMultiApplication, print)
import System.IO (putStrLn)
import Prelude hiding (print)

main :: IO ()
main = do
    let s = print (makeMultiApplication 99999 (Var "f") (Var "x"))
    putStrLn ("end for " ++ show (length s))
