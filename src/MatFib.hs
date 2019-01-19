{-# LANGUAGE BangPatterns #-}
module MatFib where

{-
Mat2x2 a b c d
  = / a b \
    \ c d /
-}
data Mat2x2 a = Mat2x2 !a !a !a !a

matMul :: (Num a) => Mat2x2 a -> Mat2x2 a -> Mat2x2 a
matMul (Mat2x2 a b c d) (Mat2x2 a' b' c' d')
  = Mat2x2 (a * a' + b * c') (a * b' + b * d')
           (c * a' + d * c') (c * b' + d * d')

matPow :: (Num a) => Mat2x2 a -> Int -> Mat2x2 a
matPow _ 0 = Mat2x2 1 0 0 1
matPow m i = loop m m (i - 1)
  where
    loop acc !_ 0 = acc
    loop acc m 1 = acc `matMul` m
    loop acc m i = case i `quotRem` 2 of
               (j,0) -> loop acc (m `matMul` m) j
               (j,_) -> loop (acc `matMul` m) (m `matMul` m) j

fib :: Int -> Integer
fib i = let Mat2x2 a b c d = matPow (Mat2x2 0 1 1 1) i
        in b

{-
/ f[n] \ = /0 1\ /f[n-1]\
\f[n+1]/ = \1 1/ \ f[n] /
-}
