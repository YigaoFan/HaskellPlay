module Util where

domain :: [(a, b)] -> [a]
domain list = [key | (key, _) <- list]