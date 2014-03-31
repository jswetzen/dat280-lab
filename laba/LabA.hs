module Main where

import Control.Parallel
import Criterion.Main
import System.Random (randoms, mkStdGen)
import Control.Monad.Par as P
import Control.Parallel.Strategies as S
import Control.Monad

--------------Given.hs------------
import Data.Complex
import Data.Array
import Data.Bits
import System.Random
import Data.Random.Normal (normals')

main :: IO ()
main = do sample <- generate2DSamplesList 100000 mX mY sdX sdY
--           print $ pfft 5 sample
--           print $ fft sample
--           print $ runPar $ parfft 5 sample
--           print $ sfft sample
          print $ pfft2 5 sample
          -- Testing if they give the same result
          -- let fsol = fft sample
          -- let ssol = sfft sample
          -- print $ fsol == ssol
-- main = print $ plscanl1 slowOp $ manyInts
-- main = print $ chscanl1 slowOp $ manyInts
-- main = print $ runPar $ pmscanl1 slowOp $ manyInts
-- main = print $ dcscanl1 slowOp $ manyInts
-- main = print $ scanl1 slowOp $ manyInts
-- main = print $ ppscanl1 slowOp $ manyInts
-- main = print $ sscanl1 slowOp $ manyInts

{-
main = benchTask2

benchTask1 :: IO ()
benchTask1 = defaultMain
  [bench "scanl1" (nf (scanl1 slowOp) aBitFewerInts),
   bench "fscanl1" (nf (fscanl1 slowOp) aBitFewerInts),
   bench "sscanl1" (nf (sscanl1 slowOp) aBitFewerInts),
   bench "ppscanl1" (nf (ppscanl1 slowOp) aBitFewerInts),
   bench "dcscanl1" (nf (dcscanl1 slowOp) aBitFewerInts),
   bench "pmscanl1" (nf (runPar.(pmscanl1 slowOp)) aBitFewerInts),
   bench "chscanl1" (nf (chscanl1 slowOp) aBitFewerInts),
   bench "plscanl1" (nf (plscanl1 slowOp) aBitFewerInts)]

benchTask2 :: IO ()
benchTask2 = do
  samples <- generate2DSamplesList 10000 mX mY sdX sdY
  defaultMain
    [bench "fft" (nf fft samples), -- A base to test against
     bench "pfft" (nf (pfft 5) samples), -- par & pseq, some speedup
     bench "pfft2" (nf (pfft2 5) samples), -- rpar & resq, makes it worse
     bench "sfft" (nf (sfft) samples),  -- Strategies (first try), really bad
     bench "spfft" (nf (spfft) samples), -- divConq with Par, the best one yet
     bench "parfft" (nf (runPar . (parfft 5)) samples)] -- Par monad, quite good
-- -}

manyInts :: [Int]
manyInts = replicate 1000 100

aBitFewerInts :: [Int]
aBitFewerInts = replicate 100 100

-- This is the original code from Mary
type Fan a = [a] -> [a]

mkFan :: (a -> a -> a) -> Fan a
mkFan op (i:is) = i:[op i k | k <- is]

pmmkFan :: NFData a => (a -> a -> a) -> [a] -> Par [a]
--pmmkFan op = return (\(i:is) -> i:parMap (op i) is)
pmmkFan op (i:is) = do liftM (i:) $ P.parMap (op i) is

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
  fros <- (pmmkFan f (last los' : ros'))
  return (init los' ++ fros)

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

--------------------------------Given.hs----------------------------------------

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
divConq :: NFData sol => (prob -> Bool)-- is the problem indivisible?
            -> (prob -> [prob])        -- split
            -> ([sol] -> sol)          -- join
            -> (prob -> sol)           -- solve a sub-problem
            -> (prob -> sol)
-- divConq = divConq'
divConq ind s j sol = \inp ->
  divConq'' sol inp ind jo sp
    where
      jo x y = j [x,y]
      sp xs = Just $ tuplify $ s xs
      tuplify (a:b:_) = (a,b)

-- Shamelessly copied from lecture 4
divConq'' ::  NFData b =>
              (a -> b)                -- function on base cases
               -> a                   -- input
               -> (a -> Bool)         -- threshold reached?
               -> (b -> b -> b)       -- combine
               -> (a -> Maybe (a,a))  -- divide
               -> b                   -- result
divConq'' f arg threshold combine divide = go arg
  where
    go arg =
      case divide arg of
        Nothing -> f arg
        Just (l0,r0) -> combine l1 r1 `using` strat
          where
            l1 = go l0
            r1 = go r0
            strat x = do r l1; r r1; return x
                where r | threshold arg = rseq
                        | otherwise = rpar

-- Task 2


-- twiddle factors
tw :: Int -> Int -> Complex Float
tw n k = cis (-2 * pi * fromIntegral k / fromIntegral n)

dft :: [Complex Float] -> [Complex Float]
dft xs = [ sum [ xs!!j * tw n (j*k) | j <- [0..n']] | k <- [0..n']]
  where
    n = length xs
    n' = n-1

-- Parallel version - par & pseq

pfft :: Int -> [Complex Float] -> [Complex Float]
pfft _ [a] = [a]
pfft 0 as = fft as
pfft n as = ls `par` rs `pseq` interleave ls rs
  where
    (cs,ds) = pbflyS as
    ls = pfft (n-1) cs
    rs = pfft (n-1) ds

pbflyS :: [Complex Float] -> ([Complex Float], [Complex Float])
pbflyS as = tws `par` (los,rts)
  where
    (ls,rs) = halve as
    los = zipWith (+) ls rs
    ros = zipWith (-) ls rs
    tws = [tw (length as) i | i <- [0..(length ls)]]
    rts = zipWith (*) ros tws

-- Parallel version - Par Monad

parfft :: Int -> [Complex Float] -> Par [Complex Float]
parfft _ [a] = return [a]
parfft 0 as = return $ fft as
parfft n as = do
    (cs,ds) <- parbflyS as
    ls <- spawn $ parfft (n-1) cs
    rs <- spawn $ parfft (n-1) ds
    ls' <- get ls
    rs' <- get rs
    return $ interleave ls' rs'

parbflyS :: [Complex Float] -> Par ([Complex Float], [Complex Float])
parbflyS as = do
    let (ls,rs) = halve as
    los <- new
    ros <- new
    fork $ put los $ zipWith (+) ls rs
    fork $ put ros $ zipWith (-) ls rs
    los' <- get los
    ros' <- get ros
    tws <- spawn $ P.parMap (tw (length as)) [0..(length ls)]
    tws' <- get tws
    let rts = zipWith (*) ros' tws'
    return (los',rts)

-- Strategies

sfft :: [Complex Float] -> [Complex Float]
sfft as = divConq''
            solve
            (5,as)
            (\(i,_) -> i<=0)
            combine
            half
  where
    half (_,(b:[])) = Nothing
    half (i,bs) = Just $ withCounter (i-1) $ bflyS bs
    withCounter i (l,r) = ((i,l),(i,r))
    combine ls rs = interleave ls rs
    solve (_,[c]) = [c]

-- Thought I did strategies first, but divConq uses Par
spfft :: [Complex Float] -> [Complex Float]
spfft [a] = [a]
spfft as = divConq
            (\(i,_) -> i<=0)
            half
            combine
            solve
            (5,as)
  where
    half (i,bs) = withCounter (i-1) $ bflyS bs
    withCounter i (l,r) = [(i,l),(i,r)]
    combine (ls:rs:_) = interleave ls rs
    solve (_,cs) = fft cs

-- rpar & rseq
pfft2 :: Int -> [Complex Float] -> [Complex Float]
pfft2 _ [a] = [a]
pfft2 0 as  = fft as
pfft2 d as  = runEval $ do
    let (cs,ds) = pbflyS2 as
    ls <- rpar $ pfft2 (d-1) cs
    rs <- rseq $ pfft2 (d-1) ds
    rseq ls
    return (interleave ls rs)

-- rpar & rseq
pbflyS2 :: [Complex Float] -> ([Complex Float], [Complex Float])
pbflyS2 as = runEval $ do
    let (ls,rs) = halve as
    los <- rpar $ zipWith (+) ls rs
    ros <- rseq $ zipWith (-) ls rs
    tws <- pMap 50 (tw (length as)) [0..(length ros)-1]
--    tws <- return $ S.parMap rseq (tw (length as)) [0..(length ros)-1]
    rts <- rpar $ zipWith (*) ros tws
    rseq los
    return (los,rts)
      where
        pMap :: Int -> (a -> b) -> [a] -> Eval [b]
        pMap _ _ []     = return []
        pMap 0 f bs     = return $ map f bs
        --pMap d f (b:bs) = do c <- rpar $ f b
        --                     cs <- pMap (d-1) f bs
        --                     --rseq c
        --                     --rseq cs
        --                     return (c:cs)
        pMap chunckSize f bs = do d <- rpar $ map f $ take chunckSize bs
                                  ds <- pMap chunckSize f $ drop chunckSize bs
                                  rseq d
                                  return $ d ++ ds

-- End parallel version

-- In case you are wondering, this is the Decimation in Frequency (DIF) 
-- radix 2 Cooley-Tukey FFT

fft :: [Complex Float] -> [Complex Float]
fft [a] = [a]
fft as = interleave ls rs
  where
    (cs,ds) = bflyS as
    ls = fft cs
    rs = fft ds

interleave :: [a] -> [a] -> [a]
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
halve :: [a] -> ([a],[a])
halve as = splitAt n' as
  where
    n' = div (length as + 1) 2
