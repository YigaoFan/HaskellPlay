{-# LANGUAGE TemplateHaskell #-}
module TypeUtil where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import qualified Test.HUnit.Lang
-- is data dataConstr = runQ [e|
--   case data of
--     dataConstr -> True
--     _          -> False
--   |]

shouldMatch :: Q Pat -> Q Exp
shouldMatch qpat = do
  shouldMatch' qpat [|pure ()|]

shouldMatch' :: Q Pat -> Q Exp -> Q Exp
shouldMatch' qpat qout = do
  ppat <- fmap pprint qpat
  [|
    \x ->
      case x of
        $qpat -> $(qout)
        _ -> do
          Test.HUnit.Lang.assertFailure
            ( "Failed to match pattern; expected: { "
                <> $(lift ppat)
                <> " }; got: "
                <> show x
            )
    |]