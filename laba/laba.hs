module Main where

import Control.Parallel
import Criterion.Main
import System.Random

main :: IO ()
main = defaultMain
  [bench "sscanl1" (nf (sscanl1 (+)) randomInts),
   bench "ppscanl1" (nf (ppscanl1 (+)) randomInts)]

randomInts :: [Integer]
randomInts =
  take 200000 (randoms (mkStdGen 13402149018394))
    :: [Integer]

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

--- Start of our own copy with modifications

-- The original code
sscanl1 :: (a -> a -> a) -> [a] -> [a]
sscanl1 _ [a] = [a]
sscanl1 f as = init los ++ ros'
  where
    (las,ras) = splitAt (cnd2 (length as)) as
    (los,ros) = (skl ff las, skl ff ras)
    ros' = ff (last los : ros)
    ff = mkFan f

-- Parallel with par and pseq
ppscanl1 :: (a -> a -> a) -> [a] -> [a]
ppscanl1 _ [a] = [a]
ppscanl1 f as = par ros (pseq los (init los ++ ros'))
  where
    (las,ras) = splitAt (cnd2 (length as)) as
    (los,ros) = (skl ff las, skl ff ras)
    ros' = ff (last los : ros)
    ff = mkFan f

