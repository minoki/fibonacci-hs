module BinetSqrt5 where
import Data.Ratio

-- Ext_sqrt5 a b = a + b * sqrt 5
data Ext_sqrt5 a = Ext_sqrt5 !a !a deriving (Eq,Show)

instance Num a => Num (Ext_sqrt5 a) where
  Ext_sqrt5 a b + Ext_sqrt5 a' b' = Ext_sqrt5 (a + a') (b + b')
  Ext_sqrt5 a b - Ext_sqrt5 a' b' = Ext_sqrt5 (a - a') (b - b')
  negate (Ext_sqrt5 a b) = Ext_sqrt5 (negate a) (negate b)
  Ext_sqrt5 a b * Ext_sqrt5 a' b' = Ext_sqrt5 (a * a' + 5 * b * b') (a * b' + b * a')
  fromInteger n = Ext_sqrt5 (fromInteger n) 0
  abs = undefined
  signum = undefined

instance Fractional a => Fractional (Ext_sqrt5 a) where
  recip (Ext_sqrt5 a b) = let s = a * a - 5 * b * b
                          in Ext_sqrt5 (a / s) (- b / s)
  fromRational x = Ext_sqrt5 (fromRational x) 0

phi, psi, sqrt5 :: Ext_sqrt5 Rational
phi = Ext_sqrt5 (1/2) (1/2)
psi = Ext_sqrt5 (1/2) (-1/2)
sqrt5 = Ext_sqrt5 0 1

fib :: Int -> Integer
fib i = case (phi^i - psi^i) / sqrt5 of
          Ext_sqrt5 x 0 | denominator x == 1 -> numerator x
          x -> error $ "calculation error: fib " ++ show i ++ " = " ++ show x
