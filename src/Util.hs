module Util where

import Data.List (partition)
import Debug.Trace (trace)
-- import IPPrint (pshow)

whileNotEmpty :: [b] -> a -> (a -> [b] -> (a, [b])) -> a
whileNotEmpty ys x f =
  go x ys
  where
    go x [] = x
    go x ys = go x' ys'
      where
        (x', ys') = f x ys

foldlFlipped :: [a] -> b -> (b -> a -> b) -> b
foldlFlipped list init f =
  foldl f init list

-- tracepshow :: Show a => a -> a
-- tracepshow x =
--   trace (pshow x) x

-- tracepshowmsg :: Show a => String -> a -> a
-- tracepshowmsg msg x =
--   trace (msg ++ pshow x) x



groupOn :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupOn _ [] = []
groupOn getKey list@(head:_) = (headKey, list1) : groupOn getKey list2
  where
    headKey = getKey head
    (list1, list2) = partition ((headKey ==) . getKey) list
