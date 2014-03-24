module Main where

import Control.Parallel
import Criterion.Main
import System.Random (randoms, mkStdGen)
import Control.Monad.Par
import Control.Parallel.Strategies

main :: IO ()
main = print $ plscanl1 slowOp $ manyInts
-- main = print $ chscanl1 slowOp $ manyInts
-- main = print $ runPar $ pmscanl1 slowOp $ manyInts
-- main = print $ dcscanl1 slowOp $ manyInts
-- main = print $ scanl1 slowOp $ manyInts
-- main = print $ ppscanl1 slowOp $ manyInts
-- main = print $ sscanl1 slowOp $ manyInts

{-
main = defaultMain
  [bench "scanl1" (nf (scanl1 slowOp) aBitFewerInts),
   bench "fscanl1" (nf (fscanl1 slowOp) aBitFewerInts),
   bench "sscanl1" (nf (sscanl1 slowOp) aBitFewerInts),
   bench "ppscanl1" (nf (ppscanl1 slowOp) aBitFewerInts),
   bench "dcscanl1" (nf (dcscanl1 slowOp) aBitFewerInts),
   bench "pmscanl1" (nf (runPar.(pmscanl1 slowOp)) aBitFewerInts),
   bench "chscanl1" (nf (chscanl1 slowOp) aBitFewerInts),
   bench "plscanl1" (nf (plscanl1 slowOp) aBitFewerInts)]
-- -}

manyInts :: [Int]
manyInts = replicate 1000 100

aBitFewerInts :: [Int]
aBitFewerInts = replicate 100 100

-- This is the original code from Mary
type Fan a = [a] -> [a]

mkFan :: (a -> a -> a) -> Fan a
mkFan op (i:is) = i:[op i k | k <- is]

pplus :: Fan Int
pplus = mkFan (+)

skl :: Fan a -> [a] -> [a]
skl _ [a] = [a]
skl f as = init los ++ ros'
  where
    (las,ras) = splitAt (cnd2 (length as)) as
    (los,ros) = (skl f las, skl f ras)
    ros' = f (last los : ros)

cnd2 :: Integral a => a -> a
cnd2 n = n - n `div` 2 -- Ceiling of n/2

cnd23 :: Integral a => a -> a
cnd23 n = n - n `div` 3 -- Ceiling of n/2

--- Start of our own copy with modifications

-- Slow operator
slowOp :: Integral a => a -> a -> a
slowOp a b | a <= 0    = b
           | otherwise = 1 + (slowOp (a-1) b)

-- Fast scanl1
fscanl1 :: (a -> a -> a) -> [a] -> [a]
fscanl1 _ []     = []
fscanl1 _ [a]    = [a]
fscanl1 f (a:b:as) = a : fscanl1 f (b':as)
  where b' = f a b

-- The original code
sscanl1 :: (a -> a -> a) -> [a] -> [a]
sscanl1 _ [a] = [a]
sscanl1 f as = init los ++ ros'
  where
    (las,ras) = splitAt (cnd2 (length as)) as
    (los,ros) = (sscanl1 f las, sscanl1 f ras)
    ros' = ff (last los : ros)
    ff = mkFan f

-- Parallel with par and pseq
ppscanl1 :: (a -> a -> a) -> [a] -> [a]
ppscanl1 _ [a] = [a]
ppscanl1 f as = ppscanl1' 5 f as

ppscanl1' :: Int -> (a -> a -> a) -> [a] -> [a]
ppscanl1' _ _ [a] = [a]
ppscanl1' 0 f as = sscanl1 f as
ppscanl1' n f as = par ros (pseq los (init los ++ ros'))
  where
    (las,ras) = splitAt (cnd2 (length as)) as
    (los,ros) = (ppscanl1' (n-1) f las, ppscanl1' (n-1) f ras)
    ros' = ff (last los : ros)
    ff = mkFan f


dcscanl1 :: (NFData a) => (a -> a -> a) -> [a] -> [a]
dcscanl1 f xs = divConq
                  (\(i,_) -> i<=0)
                  half
                  combine
                  solve
                  (5,xs)
  where
    half (i,as) = toList i $ splitAt (cnd2 (length as)) as
    toList i (las,ras) = [(i-1,las),(i-1,ras)]
    combine (los:ros:_) = init los ++ (mkFan f) (last los : ros)
    solve (_, as) = fscanl1 f as

pmscanl1 :: NFData a => (a -> a -> a) -> [a] -> Par [a]
pmscanl1 _ [a] = return [a]
pmscanl1 f as = do
  let (las,ras) = splitAt (cnd2 (length as)) as
  los <- spawn $ pmscanl1 f las
  ros <- spawn $ pmscanl1 f ras
  los' <- get los
  ros' <- get ros
  return (init los' ++ (mkFan f) (last los' : ros'))

randomInts :: [Int]
randomInts =
  take 200 (randoms (mkStdGen 13402149018394))
    :: [Int]

divConq :: NFData sol
  => (prob -> Bool) -- indivisible?
  -> (prob -> [prob]) -- split into subproblems
  -> ([sol] -> sol) -- join solutions
  -> (prob -> sol) -- solve a subproblem
  -> (prob -> sol)
divConq indiv split join f prob
  = runPar $ go prob
  where
    go prob
      | indiv prob = return (f prob)
      | otherwise = do
        sols <- parMapM go (split prob)
        return (join sols)

chscanl1 :: NFData a => (a -> a -> a) -> [a] -> [a]
chscanl1 f xs = sscanl1 f xs `using` parListChunk 20 rdeepseq

plscanl1 :: NFData a => (a -> a -> a) -> [a] -> [a]
plscanl1 f xs = sscanl1 f xs `using` parList rdeepseq
