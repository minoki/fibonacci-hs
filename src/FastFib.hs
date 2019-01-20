{-# LANGUAGE BangPatterns #-}
module FastFib where
import Data.Semigroup
import Data.Monoid

-- FastFib a b : (a, b) = (F[n-1], F[n]) for some n
-- TODO: Good name?
data FastFib a = FastFib !a !a deriving (Eq,Show)

-- fibInitial = (F[0], F[1])
fibInitial :: (Num a) => FastFib a
fibInitial = FastFib 0 1

instance (Num a) => Semigroup (FastFib a) where
  -- (F[m-1], F[m]) <> (F[n-1], F[n])
  --  ==> (F[m+n-1], F[m+n]) = (F[m-1] * F[n-1] + F[m] * F[n], F[m-1] * F[n] + F[m] * F[n-1] + F[m] * F[n])
  FastFib a b <> FastFib a' b'
    = let bb' = b * b'
      in FastFib (a * a' + bb') (a * b' + b * a' + bb')
  stimes = stimesMonoid

instance (Num a) => Monoid (FastFib a) where
  -- (F[-1], F[0])
  mempty = FastFib 1 0

fib :: Int -> Integer
fib i = case stimesMonoid i fibInitial of
          FastFib _ b -> b

fibSq :: (Num a) => FastFib a -> FastFib a
fibSq (FastFib a b) = let bb = b * b
                      in FastFib (a * a + bb) (2 * a * b + bb)

fibPow :: (Num a) => FastFib a -> Int -> FastFib a
fibPow _ 0 = mempty
fibPow m i = loop m m (i - 1)
  where
    loop acc !_ 0 = acc
    loop acc m 1 = acc <> m
    loop acc m i = case i `quotRem` 2 of
               (j,0) -> loop acc (fibSq m) j
               (j,_) -> loop (acc <> m) (fibSq m) j

fibX :: Int -> Integer
fibX i = case fibPow fibInitial i of
           FastFib _ b -> b
