{-# LANGUAGE BangPatterns #-}
module BinetPhi where
import Data.Ratio

{-
Ext_phi a b = a + b * phi
where phi is a root of
  phi^2 - phi - 1 = 0
-}
data Ext_phi a = Ext_phi !a !a deriving (Eq,Show)

instance Num a => Num (Ext_phi a) where
  Ext_phi a b + Ext_phi a' b' = Ext_phi (a + a') (b + b')
  Ext_phi a b - Ext_phi a' b' = Ext_phi (a - a') (b - b')
  negate (Ext_phi a b) = Ext_phi (negate a) (negate b)
  Ext_phi a b * Ext_phi a' b' = let bb' = b * b'
                                in Ext_phi (a * a' + bb') (a * b' + b * a' + bb')
  fromInteger n = Ext_phi (fromInteger n) 0
  abs = undefined
  signum = undefined

instance Fractional a => Fractional (Ext_phi a) where
  recip (Ext_phi a b) = let s = a * a + a * b - b * b
                        in Ext_phi ((a + b) / s) (- b / s)
  fromRational x = Ext_phi (fromRational x) 0

phi, psi :: (Num a) => Ext_phi a
phi = Ext_phi 0 1
psi = 1 - phi

fib :: Int -> Integer
fib i = case (phi^i - psi^i) / (phi - psi) of
          Ext_phi x 0 | denominator x == 1 -> numerator x
          x -> error $ "calculation error: fib " ++ show i ++ " = " ++ show x

fibI :: Int -> Integer
fibI i = case phi^i - psi^i of
          Ext_phi mx y | 2 * (- mx) == y -> - mx
          x -> error $ "calculation error: fib " ++ show i ++ " * sqrt 5 = " ++ show x
