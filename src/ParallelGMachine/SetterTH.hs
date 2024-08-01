{-# LANGUAGE TemplateHaskellQuotes #-}

module ParallelGMachine.SetterTH where
import Language.Haskell.TH (Q, Dec)
import Data.Char (toUpper)

defSetter :: String -> Q [Dec]
defSetter field@(x : rest) =
  [d| setterName a o = o { fieldName = a } |]
  where
    setterName = "set" ++ toUpper x : rest
    fieldName = field