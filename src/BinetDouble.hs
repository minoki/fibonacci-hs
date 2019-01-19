module BinetDouble where

phi, psi :: Double
phi = (1 + sqrt 5) / 2
psi = (1 - sqrt 5) / 2

fib :: Int -> Double
fib i = (phi^i - psi^i) / sqrt 5

fibP :: Int -> Double
fibP i = (phi ** fromIntegral i - psi ** fromIntegral i) / sqrt 5
