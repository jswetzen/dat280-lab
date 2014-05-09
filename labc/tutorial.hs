module Tutorial where

import Control.Monad.Par
import System.Random
import Data.List
import Criterion.Main

benchTasks :: IO ()
benchTasks = do
  seed <- newStdGen
  let rl = randomlist 1000 seed
      qs = bgroup "Quicksort" [bench "Sequential" (nf qsort rl)
                              ,bench "Parallel" (nf pqsort rl)
                              ,bench "Parallel with depth" (nf (pqsort2 5) rl)]
  --print rl
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

pqsort2 :: (NFData a, Ord a) => Int -> [a] -> [a]
pqsort2 _ [] = []
pqsort2 0 xs = qsort xs
pqsort2 d (x:xs) = runPar $ do
  lesser <- spawn $ return $ pqsort2 (d-1) $ filter (< x) xs
  greater <- spawn $ return $ pqsort2 (d-1) $ filter (>= x) xs
  l <- get lesser
  g <- get greater
  return $ l ++ [x] ++ g

