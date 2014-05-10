module Main where

import Control.Monad.Par as P
import System.Random
import Data.List
import Criterion.Main

main :: IO ()
main = benchTasks

benchTasks :: IO ()
benchTasks = do
  seed <- newStdGen
  let len = 5000
      rl = randomlist len seed
      qs = bgroup "Quicksort"
        [bench "Sequential factorial" (nf fac 5000)
        ,bench "Parallel factorial" (nf facP 5000)
        ,bench "Sequential" (nf qsort rl)
        ,bench "Parallel" (nf pqsort (take (len `div` 5) rl))
        ,bench "Parallel with depth" (nf (runPar . pqsort2 5) rl)
        ,bench "Parallel with fork" (nf (runPar . pqsort3) (take (len `div` 5) rl))
        ,bench "Parallel with fork and depth" (nf (runPar . pqsort4 5) rl)]
  defaultMain [qs]

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

{-

The Par monad provides parallelism by modeling the flow of data. Instead of
explicitly stating what should be run in parallel, the dependencies of the
computations decide how many processes there will be. The parallel computations
are written, as one would expect, in the Par monad and then the results can be
extracted using runPar.

runPar :: Par a -> a

To start computations in parallel, there is a function called fork.

fork :: Par () -> Par ()

As you can see, the fork function doesn't return any data, so instead there is
a data type for this, called IVar. An Ivar is a container for a value computed
in parallel and there are a few functions provided for using it.

new :: Par (IVar a)
put :: NFData a => IVar a -> a -> Par ()
get :: IVar a -> Par a

The new function creates an IVar, put fills it with a value and get takes it
out once it has been computed. Let's illustrate this by parallelising a simple
factorial funciton. First, a sequential version might look like this.

fac :: Integer -> Integer
fac n = product [1..n]

Say we want to split this into two parts in parallel. Then we need to declare
two new IVars, fork the left and right computation and then join them together.
Since this is such a simple example, the dependencies look like this:

  Left  Right
    \   /
    return

-}

fac :: Integer -> Integer
fac n = product [1..n]

facP :: Integer -> Integer
facP n = runPar $ do
  left <- new
  right <- new
  fork $ put left $ product [1..mid]
  fork $ put right $ product [(mid+1)..n]
  l <- get left
  r <- get right
  return $ l * r
    where mid = n `div` 2

{-
All this IVar declaring can look a bit messy, so there are some functions to
help us avoid both new and put.

spawn :: NFData a => Par a -> Par (IVar a)
spawnP :: NFData a => a -> Par (IVar a)

spawn is like fork, but it creates the IVar for you. spawnP is defined as
spawn . return, so we can call it with a pure function directly. This
simplifies our code a bit.
-}

facP' :: Integer -> Integer
facP' n = runPar $ do
  left <- spawnP $ product [1..mid]
  right <- spawnP $ product [(mid+1)..n]
  [l,r] <- mapM get [left,right]
  return $ l * r
    where mid = n `div` 2

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lesser ++ [x] ++ qsort greater
  where
    lesser = filter (< x) xs
    greater = filter (>= x) xs

pqsort :: (NFData a, Ord a) => [a] -> [a]
pqsort [] = []
pqsort (x:xs) = runPar $ do
  lesser <- spawn $ return $ pqsort $ filter (< x) xs
  greater <- spawn $ return $ pqsort $ filter (>= x) xs
  l <- get lesser
  g <- get greater
  return $ l ++ [x] ++ g

pqsort2 :: (NFData a, Ord a) => Int -> [a] -> Par [a]
pqsort2 _ [] = return []
pqsort2 0 xs = return $ qsort xs
pqsort2 d (x:xs) = do
  lesser <- spawn $ pqsort2 (d-1) $ filter (< x) xs
  greater <- spawn $ pqsort2 (d-1) $ filter (>= x) xs
  l <- get lesser
  g <- get greater
  return $ l ++ [x] ++ g

pqsort3 :: (NFData a, Ord a) => [a] -> Par [a]
pqsort3 [] = return []
pqsort3 (x:xs) = do
  [lesser,greater] <- sequence [new,new]
  fork $ do y <- pqsort3 $ filter (< x) xs
            put lesser y
  fork $ do y <- pqsort3 $ filter (>= x) xs
            put greater y
  l <- get lesser
  g <- get greater
  return $ l ++ [x] ++ g


pqsort4 :: (NFData a, Ord a) => Int -> [a] -> Par [a]
pqsort4 _ [] = return []
pqsort4 0 xs = return $ qsort xs
pqsort4 d (x:xs) = do
  [lesser,greater] <- sequence [new,new]
  fork $ do y <- pqsort4 (d-1) $ filter (< x) xs
            put lesser y
  fork $ do y <- pqsort4 (d-1) $ filter (>= x) xs
            put greater y
  l <- get lesser
  g <- get greater
  return $ l ++ [x] ++ g




