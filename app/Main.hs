module Main where
import qualified VerySlowFib
import qualified ListMemo
import qualified ListFib
import qualified IterFib
import qualified MatFib
import qualified BinetDouble
import qualified BinetSqrt5
import qualified BinetPhi
import System.Environment
import Control.Monad

data FibMethod = FibByList [Integer]
               | FibByFn (Int -> Integer)
               | FibByFnDouble (Int -> Double)

fibByMethod :: String -> FibMethod
fibByMethod "VerySlow" = FibByFn VerySlowFib.fib
fibByMethod "ListMemo" = FibByFn ListMemo.fib
fibByMethod "List" = FibByList ListFib.fib
fibByMethod "ListS" = FibByList ListFib.fib'
fibByMethod "Iter" = FibByFn IterFib.fib
fibByMethod "IterL" = FibByFn IterFib.fibL
fibByMethod "IterM" = FibByFn IterFib.fibM
fibByMethod "Mat" = FibByFn MatFib.fib
fibByMethod "BinetDouble" = FibByFnDouble BinetDouble.fib
fibByMethod "BinetDoubleP" = FibByFnDouble BinetDouble.fibP
fibByMethod "BinetSqrt5" = FibByFn BinetSqrt5.fib
fibByMethod "BinetPhi" = FibByFn BinetPhi.fib
fibByMethod "BinetPhiI" = FibByFn BinetPhi.fibI
fibByMethod _ = error "Unknown method"

main :: IO ()
main = do
  args <- getArgs
  case args of
    method:"at":m:_ -> do
      i <- readIO m
      case fibByMethod method of
        FibByFn fib -> do
          putStrLn ("F[" ++ show i ++ "] = " ++ show (fib i))
        FibByFnDouble fib -> do
          let v = fib i
          putStrLn ("F[" ++ show i ++ "] = " ++ show (round v) ++ " (" ++ show v ++ ")")
        FibByList fib -> do
          putStrLn ("F[" ++ show i ++ "] = " ++ show (fib !! i))
    method:"atS":m:_ -> do
      i <- readIO m
      case fibByMethod method of
        FibByFn fib -> do
          putStrLn ("F[" ++ show i ++ "] = " ++ show (fib i `mod` 10000))
        FibByFnDouble fib -> do
          let v = fib i
          putStrLn ("F[" ++ show i ++ "] = " ++ show (round v `mod` 10000))
        FibByList fib -> do
          putStrLn ("F[" ++ show i ++ "] = " ++ show (fib !! i `mod` 10000))
    method:"sum":m:_ -> do
      m <- readIO m
      case fibByMethod method of
        FibByFn fib -> do
          let s = sum [fib i `mod` 10000 | i <- [0..m-1]]
          print (s `mod` 10000)
        FibByFnDouble fib -> do
          let s = sum [round (fib i) `mod` 10000 | i <- [0..m-1]]
          print (s `mod` 10000)
        FibByList fib -> do
          let s = sum [x `mod` 10000 | x <- take m fib]
          print (s `mod` 10000)
    method:m:_ -> do
      m <- readIO m
      case fibByMethod method of
        FibByFn fib -> do
          forM_ [0..m-1] $ \i -> do
            putStrLn ("F[" ++ show i ++ "] = " ++ show (fib i))
        FibByFnDouble fib -> do
          forM_ [0..m-1] $ \i -> do
            let v = BinetDouble.fibP i
            putStrLn ("F[" ++ show i ++ "] = " ++ show (round v) ++ " (" ++ show v ++ ")")
        FibByList fib -> do
          forM_ (zip [0..m-1] fib) $ \(i,f) -> do
            putStrLn ("F[" ++ show i ++ "] = " ++ show f)
    "CheckBinetDouble":_ -> do
      forM_ (zip [0..100] ListFib.fib') $ \(i,f) -> do
        let b1 = BinetDouble.fib i
            f1 = round b1
        let b2 = BinetDouble.fibP i
            f2 = round b2
        if f == f1 && f == f2
          then return ()
          else do
            putStrLn ("F[" ++ show i ++ "] = " ++ show f
                      ++ ", BinetDouble: " ++ show f1 ++ " (" ++ show b1 ++ ")"
                      ++ ", BinetDoubleP: " ++ show f2 ++ " (" ++ show b2 ++ ")")
    method:_ -> do
      let m = 50
      case fibByMethod method of
        FibByFn fib -> do
          forM_ [0..m-1] $ \i -> do
            putStrLn ("F[" ++ show i ++ "] = " ++ show (fib i))
        FibByFnDouble fib -> do
          forM_ [0..m-1] $ \i -> do
            let v = BinetDouble.fibP i
            putStrLn ("F[" ++ show i ++ "] = " ++ show (round v) ++ " (" ++ show v ++ ")")
        FibByList fib -> do
          forM_ (zip [0..m-1] fib) $ \(i,f) -> do
            putStrLn ("F[" ++ show i ++ "] = " ++ show f)
    _ -> putStrLn "Usage: fibonacci-hs-exe method [at] [n]"
