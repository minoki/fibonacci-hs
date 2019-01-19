{-# LANGUAGE BangPatterns #-}
module ListFib where

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = let z = f x y
                           in z `seq` (z : zipWith' f xs ys)

fib' :: [Integer]
fib' = 0 : 1 : zipWith' (+) fib' (tail fib')
