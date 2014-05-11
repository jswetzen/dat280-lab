{-# LANGUAGE TypeOperators #-}
module Main where
import System.Random
import Data.List
import Data.Array.Repa as R
import Data.Array.Repa.Repr.Unboxed
import Data.Maybe as M
import System.Environment
import Criterion.Main


main :: IO ()
main = benchTasks
--main = do
--  args <- getArgs
--  let seed = mkStdGen 12934871823479128347
--      len = 20000
--      rs = randomlist len seed
--      arr = arrTup len rs
--      lst = listTup rs
--  case args of
--    ["-par"] -> print $ buySellP $ delay arr
--    _        -> print $ buySellSeq'' lst

benchTasks :: IO ()
benchTasks = do
  let seed = mkStdGen 12934871823479128347
      len = 20000
      rs = randomlist len seed
      arr = arrTup len rs
      lst = listTup rs
      repa = bgroup "Repa"
        [bench "Parallel" (nf buySellP $ delay arr)
        ,bench "Seqential" (nf buySellSeq'' lst)]
  defaultMain [repa]

runParallel :: Bool
runParallel = False

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

data State = State { buy :: (Int, Int) -- (value, position)
                   , sell :: (Int, Int)
                   , mini :: (Int, Int)
                   } deriving (Show)

exampleList :: [Int]
exampleList = [0,0,2,9,8,10,1,10]

exampleListTup :: [(Int, Int, Int)]
exampleListTup = Prelude.zip3 ([0..]::[Int]) ([0..]::[Int]) exampleList

-- Applying buySell should produce (1,5,10)
exampleProblem :: Array U (Z :. Int) (Int, Int, Int)
exampleProblem = fromListUnboxed (Z :. (8::Int)) exampleListTup

listTup :: [Int] -> [(Int, Int, Int)]
listTup = Prelude.zip3 ixs ixs
  where ixs = [0..]::[Int]


arrTup :: Int -> [Int] -> Array U (Z :. Int) (Int, Int, Int)
arrTup len = fromListUnboxed (Z :. (len::Int)) . listTup

----------------
-- Sequential --
----------------

-------------
-- Try One --
-------------

buySellSeq :: Array U DIM1 Int -> (Int, Int, Int)
buySellSeq arr = buySellSeq' state 1 lst
  where (x:lst) = toList arr
        state   = State (x, 0) (x, 0) (x, 0)

buySellSeq' :: State -> Int -> [Int] -> (Int, Int, Int)
buySellSeq' s _ [] = let (bVal, bPos) = buy s
                         (sVal, sPos) = sell s
                     in  (bPos, sPos, sVal - bVal)
buySellSeq' s p (x:xs)
  | (x-mVal) > (sVal-bVal) = buySellSeq' (s {buy=mini s, sell=(x,p)}) (p+1) xs
  | x <= mVal = buySellSeq' (s {mini=(x,p)}) (p+1) xs
  | otherwise = buySellSeq' s (p+1) xs
    where
      (mVal, _) = mini s
      (sVal, _) = sell s
      (bVal, _) = buy s

-------------
-- Try Two --
-------------

interleave :: [a] -> [a] -> [a]
interleave [] xs = xs
interleave (x:xs) ys = x:interleave ys xs

odds :: [a] -> [a]
odds (_:x:xs) = x:odds xs
odds (_:xs) = odds xs
odds [] = []

evens :: [a] -> [a]
evens (x:_:xs) = x:evens xs
evens (x:xs) = x:evens xs
evens [] = []

scanOp :: (a -> a -> a) -> a -> [a] -> [a]
scanOp _ ident [_] = [ident]
scanOp op ident as = let
    e = evens as
    o = odds as
    s = scanOp op ident $ Prelude.zipWith op e o
  in interleave s $ Prelude.zipWith op s e

buySellSeq'' :: [(Int, Int, Int)] -> (Int, Int, Int)
buySellSeq'' as = maximumS $ Prelude.zipWith minus as minscan
  where
    minscan = scanOp min' (head as) as

maximumS :: [(Int, Int, Int)] -> (Int, Int, Int)
maximumS as = foldr max' (head as) as

--------------
-- Parallel --
--------------

everyOther :: ((DIM1 -> a) -> DIM1 -> a) -> Array D DIM1 a -> Array D DIM1 a
everyOther fun arr = traverse arr halfsize fun
  where halfsize (Z :. i) = Z :. i `div` 2

evensFun :: (DIM1 -> a) -> DIM1 -> a
evensFun ixf (Z :. i) = ixf (ix1 (2*i))

oddsFun :: (DIM1 -> a) -> DIM1 -> a
oddsFun ixf (Z :. i) = ixf (ix1 (2*i+1))

scanOpP :: (Unbox a) => (a -> a -> a) -> a ->
  Array D DIM1 a ->
  Array D DIM1 a
scanOpP op ident as =
  if size (extent as) == 1
  then fromFunction unitDim (const ident)
  else let
    e = everyOther evensFun as
    o = everyOther oddsFun as
    s = scanOpP op ident $ R.zipWith op e o
  in interleave2 s $ R.zipWith op s e

buySellP :: Array D DIM1 (Int, Int, Int) -> (Int, Int, Int)
buySellP arr = maximumP $ R.zipWith minus arr minscan
  where
    minscan = scanOpP min' (arr ! (Z :. 0)) $ delay arr

minus :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
minus (_, s1, v1) (b2, _, v2) = (b2, s1, v1-v2)

min' :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
min' t1@(b1, _, p1) t2@(b2, _, p2)
  | p1 < p2 = t1
  | p1 > p2 = t2
  | otherwise =
    if b1 > b2
    then t1
    else t2

max' :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
max' t1@(_, s1, p1) t2@(_, s2, p2)
  | p1 > p2 = t1
  | p1 < p2 = t2
  | otherwise =
    if s1 > s2
    then t2
    else t1

-- fromJust is potentially unsafe, but should always give an answer
-- And, we like to live dangerously
maximumP :: Array D DIM1 (Int, Int, Int) -> (Int, Int, Int)
maximumP arr = fromJust $ foldAllP max' (arr ! (Z :. 0)) arr

