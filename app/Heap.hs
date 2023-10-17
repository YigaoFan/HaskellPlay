module Heap where

import Prelude hiding (lookup)

type Addr = Int
type Heap a = (Int, [Int], [(Int, a)])

lookup :: (Eq k) => [(k, v)] -> k -> v -> v
lookup [] k' defaultValue = defaultValue
lookup ((k, v) : xs) k' defaultValue
  | k == k' = v
  | k /= k' = lookup xs k' defaultValue

initHeap :: Heap a
initHeap = (0, [1..], [])
heapLookup :: Heap a -> Addr -> a
heapLookup (_, _, addrObjs) addr = lookup addrObjs addr (error ("don't have the addr" ++ show addr))
heapAlloc :: Heap a -> a -> (Heap a, Addr)
heapAlloc (size, next : free, addrObjs) a = ((size + 1, free, (next, a) : addrObjs), next)
-- replace first item while predicate is True
replaceWhile :: (a -> Bool) -> a -> [a] -> [a]
replaceWhile pred a [] = []
replaceWhile pred a (x : xs)
  | pred x = a : xs
  | otherwise = x : replaceWhile pred a xs

heapUpdate :: Heap a -> Addr -> a -> Heap a
heapUpdate (size, free, addrObjs) addr a =
  (size, free, replaceWhile ((addr==) . fst) (addr, a) addrObjs)