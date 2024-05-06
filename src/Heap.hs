module Heap where

import Prelude hiding (lookup)
import GHC.Stack (HasCallStack, prettyCallStack)

type Addr = Int
type Heap a = (Int, [Int], [(Int, a)])

lookup :: (Eq k) => [(k, v)] -> k -> v -> v
lookup [] k' defaultValue = defaultValue
lookup ((k, v) : xs) k' defaultValue
  | k == k' = v
  | k /= k' = lookup xs k' defaultValue

addrsOf :: Heap a -> [Int]
addrsOf (_, _, items) = map fst items

initHeap :: Heap a
initHeap = (0, [1..], [])
heapLookup :: HasCallStack => Heap a -> Addr -> a
heapLookup (_, _, addrObjs) addr = lookup addrObjs addr (error ("don't have the addr " ++ show addr))
heapAlloc :: Heap a -> a -> (Heap a, Addr)
heapAlloc (size, next : free, addrObjs) a = ((size + 1, free, (next, a) : addrObjs), next)
-- replace first item while predicate is True, must have one item satisfy pred
replaceWhile :: (a -> Bool) -> a -> [a] -> [a]
replaceWhile pred a [] = error "no item satisfy pred"
replaceWhile pred a (x : xs)
  | pred x = a : xs
  | otherwise = x : replaceWhile pred a xs

heapUpdate :: Heap a -> Addr -> a -> Heap a
heapUpdate (size, free, addrObjs) addr a =
  (size, free, replaceWhile ((addr==) . fst) (addr, a) addrObjs)

heapFree :: Heap a -> Addr -> Heap a
heapFree heap@(size, free, addrObjs) addr =
  if any ((addr==) . fst) addrObjs
    then (size - 1, addr : free, filter ((addr/=) . fst) addrObjs)
    else heap