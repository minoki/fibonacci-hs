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

instance (Num a) => Monoid (FastFib a) where
  -- (F[-1], F[0])
  mempty = FastFib 1 0

fib :: Int -> Integer
fib i = case stimesMonoid i fibInitial of
          FastFib _ b -> b
