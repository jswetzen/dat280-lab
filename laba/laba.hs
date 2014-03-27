module Main where

import Control.Parallel
import Criterion.Main
import System.Random (randoms, mkStdGen)
import Control.Monad.Par
import Control.Parallel.Strategies

--------------Given.hs------------
import Data.Complex
import Data.Array
import Data.Bits
import System.Random
import Data.Random.Normal (normals')
--import Criterion.Main
--import Control.Parallel
--import Control.Monad.Par  
--import Control.Parallel.Strategies

main :: IO ()
main = do
  samples <- generate2DSamplesList 100000 mX mY sdX sdY
  let fout = fft samples
  let pout = pfft 20 samples
  -- print fout
  print pout
-- main = print $ plscanl1 slowOp $ manyInts
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
dcscanl1 f xs = divConq'
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

divConq' :: NFData sol
  => (prob -> Bool) -- indivisible?
  -> (prob -> [prob]) -- split into subproblems
  -> ([sol] -> sol) -- join solutions
  -> (prob -> sol) -- solve a subproblem
  -> (prob -> sol)
divConq' indiv split join f prob
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

---------------------------------------Given.hs---------------------------------

-- file given.hs for use with Lab 1 Part 1 of the Chalmers PFP Course
-- Please write your names in the file if submitting it


-- generating input for FFT or DFT. Borrowed from Simon Marlow I believe.
mX, mY, sdX, sdY :: Float
mX = 0
mY = 0
sdX = 0.5
sdY = 1.5    

generate2DSamplesList :: Int           -- number of samples to generate
                  -> Float -> Float    -- X and Y mean
                  -> Float -> Float    -- X and Y standard deviations
                  -> IO [Complex Float]
generate2DSamplesList n mx my sdx sdy = do
  gen <- getStdGen
  let (genx, geny) = split gen
      xsamples = normals' (mx,sdx) genx
      ysamples = normals' (my,sdy) geny
  return $ zipWith (:+) (take n xsamples) ysamples




-- Task 1
divConq :: (prob -> Bool)              -- is the problem indivisible?
            -> (prob -> [prob])        -- split
            -> ([sol] -> sol)          -- join
            -> (prob -> sol)           -- solve a sub-problem
            -> (prob -> sol)

divConq indiv split join f prob = undefined



-- Task 2


-- twiddle factors
tw :: Int -> Int -> Complex Float
tw n k = cis (-2 * pi * fromIntegral k / fromIntegral n)

dft :: [Complex Float] -> [Complex Float]
dft xs = [ sum [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]
  where
    n = length xs
    n' = n-1



-- In case you are wondering, this is the Decimation in Frequency (DIF) 
-- radix 2 Cooley-Tukey FFT

fft :: [Complex Float] -> [Complex Float]
fft [a] = [a]
fft as = interleave ls rs
  where
    (cs,ds) = bflyS as
    ls = fft cs
    rs = fft ds

-- Parallel version


pfft :: Int -> [Complex Float] -> [Complex Float]
pfft _ [a] = [a]
pfft 0 as = fft as
pfft n as = runEval $ do
  (cs,ds) <- pbflyS as
  ls <- rpar $ pfft (n-1) cs
  rs <- rpar $ pfft (n-1) ds
  return $ interleave ls rs

pbflyS :: [Complex Float] -> Eval ([Complex Float], [Complex Float])
pbflyS as = do
  (ls,rs) <- rpar $ halve as
  los <- rpar $ zipWith (+) ls rs
  ros <- rpar $ zipWith (-) ls rs
  rts <- rpar $ zipWith (*) ros [tw (length as) i | i <- [0..(length ros) - 1]]
  return (los,rts)

-- End parallel version

interleave [] bs = bs
interleave (a:as) bs = a : interleave bs as

bflyS :: [Complex Float] -> ([Complex Float], [Complex Float])
bflyS as = (los,rts)
  where
    (ls,rs) = halve as
    los = zipWith (+) ls rs
    ros = zipWith (-) ls rs
    rts = zipWith (*) ros [tw (length as) i | i <- [0..(length ros) - 1]]


-- missing from original file
halve as = splitAt n' as
  where
    n' = div (length as + 1) 2
