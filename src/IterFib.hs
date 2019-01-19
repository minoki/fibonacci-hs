{-# LANGUAGE BangPatterns #-}
module IterFib where

fib :: Int -> Integer
fib i = loop i 0 1
  where
    loop 0 a !b = a
    loop i a b = loop (i - 1) b (a + b)

fibL :: Int -> Integer
fibL i = loop i 0 1
  where
    loop 0 a b = a
    loop i a b = loop (i - 1) b (a + b)

fibM :: Int -> Integer
fibM 0 = 0
fibM i = loop i 0 1
  where
    loop 1 a b = b
    loop i a b = loop (i - 1) b (a + b)
