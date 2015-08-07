module Haskell where

import Data.List

qsort [] = []
qsort (x:xs) = qsort [a | a<-xs, a<=x] ++ [x] ++ qsort [b | b<-xs, b>x]

asd a = (\x xs -> xs ++ [a x])

evens ls = filter even ls

squares n = map (^2) [1..n]

sumSquares n = sum $ squares n

squares2 m n = map (^2) (take m [(succ n)..])

sumSquares2 n = sum . uncurry squares2 $ (n,n)

coords m n = [(x,y)|x<-[0..m], y<-[0..n]]

