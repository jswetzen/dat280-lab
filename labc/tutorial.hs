module Main where

import Control.Monad.Par
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
        [bench "Sequential" (nf qsort rl)
        ,bench "Parallel" (nf pqsort (take (len `div` 5) rl))
        ,bench "Parallel with depth" (nf (runPar . pqsort2 5) rl)
        ,bench "Parallel with fork" (nf pqsort3 (take (len `div` 5) rl))
        ,bench "Parallel with fork and depth" (nf (pqsort4 5) rl)]
  defaultMain [qs]

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

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

pqsort3 :: (NFData a, Ord a) => [a] -> [a]
pqsort3 [] = []
pqsort3 (x:xs) = runPar $ do
  [lesser,greater] <- sequence [new,new]
  fork $ put lesser $ pqsort3 $ filter (< x) xs
  fork $ put greater $ pqsort3 $ filter (>= x) xs
  l <- get lesser
  g <- get greater
  return $ l ++ [x] ++ g


pqsort4 :: (NFData a, Ord a) => Int -> [a] -> [a]
pqsort4 _ [] = []
pqsort4 0 xs = qsort xs
pqsort4 d (x:xs) = runPar $ do
  [lesser,greater] <- sequence [new,new]
  fork $ put lesser $ pqsort4 (d-1) $ filter (< x) xs
  fork $ put greater $ pqsort4 (d-1) $ filter (>= x) xs
  l <- get lesser
  g <- get greater
  return $ l ++ [x] ++ g
