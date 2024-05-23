{-# LANGUAGE TemplateHaskell #-}
module TypeUtil where

import Language.Haskell.TH ( runQ )

is data dataConstr = runQ [e|
  case data of
    dataConstr -> True
    _          -> False
  |]