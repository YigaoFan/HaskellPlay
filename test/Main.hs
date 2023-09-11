-- module Test where

-- import Control.Exception (assert)
-- import Lexer (lex)
-- import Prelude hiding (lex)

-- testLex0 :: String
-- testLex0 = assert (not $ null $ lex "Hello" 0) "pass"

-- testLex1 :: String
-- testLex1 = assert (not $ null $ lex "World" 0) "pass"

-- test :: String
-- test = do
--   testLex0
--   testLex1