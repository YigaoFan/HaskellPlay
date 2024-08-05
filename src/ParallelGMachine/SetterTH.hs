{-# LANGUAGE TemplateHaskellQuotes #-}

module ParallelGMachine.SetterTH where
import Language.Haskell.TH (Q, Dec (FunD), Exp (RecUpdE, VarE), Clause (Clause), Body (NormalB), mkName, Pat (VarP))
import Data.Char (toUpper)

-- 要不要 Q Dec，加不加有什么区别
defineSetter :: String -> Q [Dec]
defineSetter field@(x : rest) =
  pure [FunD (mkName setterName) [Clause [VarP (mkName "v"), VarP (mkName "o")] (NormalB (RecUpdE (VarE (mkName "o")) [(mkName field, VarE (mkName "v"))])) []]]
  where
    setterName = "set" ++ toUpper x : rest

-- 下面这种插值式的应该怎么写
-- f = [| pi + $(varE (mkName "pi")) |]
-- define :: String -> Q [Dec]
-- define field@(x : rest) =
--   [d| $(mkName setterName) v o = o { $(mkName field) = v }|]
--   where setterName = "set" ++ toUpper x : rest

-- 如果要读取一些类型信息，比如某个类型里是否包含某个 field，据此生成代码，这个能实现吗？