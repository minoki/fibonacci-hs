module FastDoubling where

-- @fastDoubling n@ returns @(fib n, fib (n+1))@
fastDoubling :: Int -> (Integer, Integer)
fastDoubling 0 = (0, 1) -- (F[0], F[1])
fastDoubling 1 = (1, 1) -- (F[1], F[2])
fastDoubling i                             -- let j := floor (i/2)
  = let (a, b) = fastDoubling (i `quot` 2) -- (F[j], F[j+1])
    in if even i
       then (a * (2 * b - a), a * a + b * b) -- (F[2j], F[2j+1])
       else (a * a + b * b, b * (2 * a + b)) -- (F[2j+1], F[2j+2])

fib :: Int -> Integer
fib i = fst (fastDoubling i)
