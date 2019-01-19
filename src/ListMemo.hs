module ListMemo where

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fibList !! (n - 2) + fibList !! (n - 1)

fibList = map fib [0..]
