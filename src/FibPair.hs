{-# LANGUAGE BangPatterns #-}
module FibPair where
import Data.Semigroup
import Data.Monoid

-- FibPair a b : (a, b) = (F[n], F[n+1]) for some n
data FibPair a = FibPair !a !a deriving (Eq,Show)

instance (Num a) => Semigroup (FibPair a) where
  -- (F[m], F[m+1]) <> (F[n], F[n+1])
  --  ==> (F[m+n], F[m+n+1]) = (F[m] * F[n+1] + F[m+1] * F[n] - F[m] * F[n], F[m] * F[n] + F[m+1] * F[n+1])
  FibPair a b <> FibPair a' b'
    = FibPair (a * b' + (b - a) * a') (a * a' + b * b')
  stimes = stimesMonoid

instance (Num a) => Monoid (FibPair a) where
  -- (F[0], F[1])
  mempty = FibPair 0 1

-- fibOne = (F[1], F[2])
fibOne :: (Num a) => FibPair a
fibOne = FibPair 1 1

-- fibPair i = (F[i], F[i+1])
fibPair :: Int -> FibPair Integer
fibPair i = stimesMonoid i fibOne

fib :: Int -> Integer
fib i = case fibPair i of
          FibPair a _ -> a

-----

-- (F[n], F[n+1]) <> (F[n], F[n+1])
--  ==> (F[2n], F[2n+1]) = (2 * F[n] * F[n+1] - F[n] * F[n], F[n] * F[n] + F[n+1] * F[n+1])
fibSq :: (Num a) => FibPair a -> FibPair a
fibSq (FibPair a b) = FibPair (a * (2 * b - a)) (a * a + b * b)

fibPow :: (Num a) => FibPair a -> Int -> FibPair a
fibPow _ 0 = mempty
fibPow m i = loop m m (i - 1)
  where
    loop acc !_ 0 = acc
    loop acc m 1 = acc <> m
    loop acc m i = case i `quotRem` 2 of
               (j,0) -> loop acc (fibSq m) j
               (j,_) -> loop (acc <> m) (fibSq m) j

fibX :: Int -> Integer
fibX i = case fibPow fibOne i of
           FibPair a _ -> a

-----

fastDoubling :: Int -> FibPair Integer
fastDoubling 0 = FibPair 0 1
fastDoubling 1 = FibPair 1 1
fastDoubling i                        -- let j := floor (i/2)
  = let p = fastDoubling (i `quot` 2) -- (F[j], F[q+1])
        q@(FibPair a b) = p <> p      -- (F[2j], F[2j+1])
    in if even i
       then q
       else FibPair b (a + b)         -- (F[2j+1], F[2j+2])

fibD :: Int -> Integer
fibD i = case fastDoubling i of
           FibPair a _ -> a
