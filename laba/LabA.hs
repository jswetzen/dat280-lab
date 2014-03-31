module Main where

import Control.Parallel
import Control.DeepSeq
import Criterion.Main
import System.Random (randoms, mkStdGen)
import Control.Monad.Par
import Control.Parallel.Strategies hiding (parMap)
import Control.Monad

--------------Given.hs------------
import Data.Complex
import Data.Array
import Data.Bits
import System.Random
import Data.Random.Normal (normals')

main :: IO ()
-- main = do sample <- generate2DSamplesList 100000 mX mY sdX sdY
--           print $ pfft 5 sample
--           print $ fft sample
--           print $ runPar $ parfft 5 sample
--           print $ sfft sample
          -- print $ pfft2 5 sample
          -- Testing if they give the same result
          -- let fsol = fft sample
          -- let psol = pfft 5 sample
          -- let p2sol = pfft2 5 sample
          -- print $ fsol == psol && fsol == p2sol
-- main = print $ plscanl1 slowOp $ manyInts
-- main = print $ runPar $ pmscanl1 slowOp $ manyInts
-- main = print $ dcscanl1 slowOp $ manyInts
-- main = print $ scanl1 slowOp $ manyInts
-- main = print $ ppscanl1 slowOp $ manyInts
-- main = print $ sscanl1 slowOp $ manyInts

-- {-
main = benchTasks

benchTasks :: IO ()
benchTasks = do
  samples <- generate2DSamplesList 5000 mX mY sdX sdY
  let task1_seq = bgroup "Sequential" [bench "Prelude's scanl1" (nf (scanl1 slowOp) aBitFewerInts), bench "Mary's recursive scanl1" (nf (sscanl1 slowOp) aBitFewerInts)]
      task1_par = bgroup "Parallel"
       [bench "par & pseq" (nf (ppscanl1 slowOp) aBitFewerInts),
        bench "Divide & conquer" (nf (dcscanl1 slowOp) aBitFewerInts),
        bench "Strategies, parList" (nf (plscanl1 slowOp) aBitFewerInts),
        bench "Par Monad" (nf (runPar.(pmscanl1 slowOp)) aBitFewerInts)]
      task1 = bgroup "Task 1" [task1_seq,task1_par]
      task2_seq = bgroup "Sequential" [bench "Given fft" (nf fft samples)] -- A base to test against
      task2_par = bgroup "Parallel" $
       [bench "par & pseq" (nf (pfft 5) samples), -- par & pseq, some speedup
        bench "rpar & rseq" (nf (pfft2 5) samples), -- rpar & resq, makes it worse
        bench "Bad strategies try" (nf (sfft) samples),  -- Strategies (first try), really bad
        bench "Simon Marlow's divConq with Par" (nf (spfft) samples), -- divConq with Par, the best one yet
        bench "Par Monad" (nf (runPar . (parfft 5)) samples)] -- Par monad, quite good
      task2 = bgroup "Task 2" [task2_seq,task2_par]
  defaultMain
    [task1, task2] -- The rest of the tests
-- -}

manyInts :: [Int]
manyInts = replicate 1000 100

aBitFewerInts :: [Int]
aBitFewerInts = replicate 220 100

-- This is the original code from Mary
type Fan a = [a] -> [a]

-- Make a fanout
mkFan :: (a -> a -> a) -> Fan a
mkFan op (i:is) = i:[op i k | k <- is]

-- Make a fanout, parallel version
pmmkFan :: NFData a => (a -> a -> a) -> [a] -> Par [a]
pmmkFan op (i:is) = do liftM (i:) $ parMap (op i) is

skl :: Fan a -> [a] -> [a]
skl _ [a] = [a]
skl f as = init los ++ ros'
  where
    (las,ras) = splitAt (cnd2 (length as)) as
    (los,ros) = (skl f las, skl f ras)
    ros' = f (last los : ros)

cnd2 :: Integral a => a -> a
cnd2 n = n - n `div` 2 -- Ceiling of n/2

--- Start of our own copy with modifications

-- Slow operator
slowOp :: Integral a => a -> a -> a
slowOp a b | a <= 0    = b
           | otherwise = 1 + (slowOp (a-1) b)

-- The code stolen from Mary's paper
sscanl1 :: (a -> a -> a) -> [a] -> [a]
sscanl1 _ [a] = [a]
sscanl1 f as = init los ++ ros'
  where
    (las,ras) = splitAt (cnd2 (length as)) as
    (los,ros) = (sscanl1 f las, sscanl1 f ras)
    ros' = ff (last los : ros)
    ff = mkFan f

-- Parallel with par and pseq
ppscanl1 :: NFData a => (a -> a -> a) -> [a] -> [a]
ppscanl1 _ [a] = [a]
ppscanl1 f as = ppscanl1' 5 f as

ppscanl1' :: NFData a => Int -> (a -> a -> a) -> [a] -> [a]
ppscanl1' _ _ [a] = [a]
ppscanl1' 0 f as = sscanl1 f as
ppscanl1' n f as = rnf ros `par` los `pseq` init los ++ ros'
  where
    (las,ras) = splitAt (cnd2 (length as)) as
    (los,ros) = (ppscanl1' (n-1) f las, ppscanl1' (n-1) f ras)
    ros' = ff (last los : ros)
    ff = mkFan f

-- Divide and conquer with Simon Marlow's Par-based divConq
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
    solve (_, as) = scanl1 f as

-- Strategies, with parList
plscanl1 :: NFData a => (a -> a -> a) -> [a] -> [a]
plscanl1 f xs = sscanl1 f xs `using` parList rdeepseq

-- Pure Par implementation, with a parallel pmmkFan
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
pfft n as = rnf ls `par` rnf rs `pseq` interleave ls rs
  where
    (cs,ds) = pbflyS as
    ls = pfft (n-1) cs
    rs = pfft (n-1) ds

pbflyS :: [Complex Float] -> ([Complex Float], [Complex Float])
pbflyS as = tws `par` los `par` ros `seq` (los,rts)
  where
    (l,(ls,rs)) = halve as
    los = zipWith (+) ls rs
    ros = zipWith (-) ls rs
    tws = map (tw (length as)) [0..l-1]
    rts = zipWith (*) ros tws

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

-- rpar & rseq
pfft2 :: Int -> [Complex Float] -> [Complex Float]
pfft2 _ [a] = [a]
pfft2 0 as  = fft as
pfft2 d as  = runEval $ do
    (cs,ds) <- pbflyS2 as
    ls <- rpar $ pfft2 (d-1) cs
    rs <- rseq $ pfft2 (d-1) ds
    rseq ls
    return $ interleave ls rs

pbflyS2 :: [Complex Float] -> Eval ([Complex Float], [Complex Float])
pbflyS2 as = do
    let (l,(ls,rs)) = halve as
    los <- rpar $ zipWith (+) ls rs
    ros <- rpar $ zipWith (-) ls rs
    tws <- return $ ((`using` parListChunk 10 rdeepseq) . map (tw la)) [0..l-1]
    rseq ros
    rts <- rseq $ zipWith (*) ros tws
    rseq los
    return (los,rts)
      where
        la = length as

-- divConq from Simon Marlow's lecture, it uses Par
spfft :: [Complex Float] -> [Complex Float]
spfft [a] = [a]
spfft as = divConq'
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
    let (l,(ls,rs)) = halve as
    los <- new
    ros <- new
    fork $ put los $ zipWith (+) ls rs
    fork $ put ros $ zipWith (-) ls rs
    tws <- spawn $ parMap (tw (length as)) [0..l]
    los' <- get los
    ros' <- get ros
    tws' <- get tws
    let rts = zipWith (*) ros' tws'
    return (los',rts)

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
    (_,(ls,rs)) = halve as
    los = zipWith (+) ls rs
    ros = zipWith (-) ls rs
    rts = zipWith (*) ros [tw (length as) i | i <- [0..(length ros) - 1]]

-- missing from original file
halve :: [a] -> (Int,([a],[a]))
halve as = (n',splitAt n' as)
  where
    n' = div (length as + 1) 2
