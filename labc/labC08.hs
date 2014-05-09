{-# LANGUAGE TypeOperators #-}
module Main where
import Data.Array.Repa as R
import Data.Maybe as M

main :: IO ()
main = print "hii ^^"
-- main = do
  -- res <- foldAllP foldFun foldBase exampleProblem'
  -- print res

-- solutionTest :: Monad m => m (BuySell, Min, Current)
-- solutionTest = foldAllP foldFun foldBase exampleProblem'

data State = State { buy :: (Int, Int) -- (value, position)
                   , sell :: (Int, Int)
                   , mini :: (Int, Int)
                   } deriving (Show)

exampleList :: [Int]
exampleList = [0,0,2,9,8,10,1,10]
-- Applying buySell should produce (1,5,10)
exampleProblem :: Array U DIM1 Int
exampleProblem = fromListUnboxed (Z :. (8::Int)) exampleList

-- [((buy_pos, sell_pos), (min_val, min_pos), (pos, profit))]
-- exIds :: [(BuySell, Min, Current)]
-- exIds = zip3 (zip (repeat 0) (repeat 0)) (zip (repeat 0) (repeat 0)) (zip [0..] exampleList)

-- exampleProblem' :: Array U DIM1 (BuySell, Min, Current)
-- exampleProblem' = fromListUnboxed (Z :. (8::Int)) exIds

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

-- type BuySell = (Int, Int)
-- type Min = (Int, Int)
-- type Current = (Int, Int) -- current

-- -- [((buy_pos, sell_pos), (min_val, min_pos), (pos, profit))]
-- exIdsFun :: [Int] -> [(BuySell, Min, Current)]
-- exIdsFun xs = zip3 (zip (repeat 0) (repeat 0)) (zip (repeat 0) (repeat 0)) (zip [0..] xs)

-- foldBase :: (BuySell, Min, Current)
-- foldBase = ((0, 0), (0, 0), (0, 0))

-- foldBaseArr :: Array U DIM1 (BuySell, Min, Current) -> (BuySell, Min, Current)
-- foldBaseArr arr = arr ! (Z :. 0)

-- foldFun :: (BuySell, Min, Current) ->
--            (BuySell, Min, Current) ->
--            (BuySell, Min, Current)
-- foldFun a1@(_, _, (pos, _))
--         a2@(_, _, (pos1, _)) =
--   if pos > pos1
--   then foldFunOrd a2 a1
--   else foldFunOrd a1 a2

-- foldFunOrd :: (BuySell, Min, Current) ->
--               (BuySell, Min, Current) ->
--               (BuySell, Min, Current)
-- foldFunOrd ((bp, sp), (mv, mp), (_, prof))
--         ((_, _), (_, _), (cpos, cval))
--   | (cval-mv) > prof = ((mp,cpos), (mv,mp), (cpos,cval-mv))
--   | cval <= mv = ((bp,sp), (cval,cpos), (cpos,prof))
--   | otherwise = ((bp,sp), (mv,mp), (cpos,prof))

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

buySellSeq'' :: [Int] -> Int
buySellSeq'' as = maximum $ Prelude.zipWith (-) as minscan
  where
    minscan = scanOp min maxBound as


pairwiseAdd :: Array U DIM1 Int -> Array D DIM1 Int
pairwiseAdd arr = traverse arr halfsize indexfun
  where halfsize (Z :. i) = Z :. i `div` 2
        indexfun ixf (Z :. i) = ixf (ix1 (2*i)) + ixf (ix1 $ 2*i+1)

everyOther :: ((DIM1 -> a) -> DIM1 -> a) -> Array D DIM1 a -> Array D DIM1 a
everyOther fun arr = traverse arr halfsize fun
  where halfsize (Z :. i) = Z :. i `div` 2

evensFun :: (DIM1 -> a) -> DIM1 -> a
evensFun ixf (Z :. i) = ixf (ix1 (2*i))

oddsFun :: (DIM1 -> a) -> DIM1 -> a
oddsFun ixf (Z :. i) = ixf (ix1 (2*i+1))

scanOpP :: (a -> a -> a) -> a ->
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

buySellP :: Array D DIM1 Int -> Int
buySellP arr = maximumP $ R.zipWith (-) arr minscan
  where
    minscan = scanOpP min (arr ! (Z :. 0)) $ delay arr

-- fromJust is potentially unsafe, but should always give an answer
-- And, we like to live dangerously
maximumP :: Array D DIM1 Int -> Int
maximumP arr = fromJust $ foldAllP max (arr ! (Z :. 0)) arr


exampleList2 :: [Tuple]
exampleList2 = zipTuple ([0..] :: [Int]) ([0..] :: [Int]) exampleList

zipTuple :: [Int] -> [Int] -> [Int] -> [Tuple]
zipTuple [] [] [] = []
zipTuple (a:as) (b:bs) (c:cs) = T (a,b,c) : zipTuple as bs cs
zipTuple _ _ _ = []

-- The tuple type is meant to describe the buy position, sell position and the
-- max profit outcome.
newtype Tuple = T (Int, Int, Int) deriving Eq
instance Ord Tuple where
  T (_,_,c1) `compare` T (_,_,c2) = c1 `compare` c2
  T (_,_,c1) >  T (_,_,c2) = c1 > c2
  T (_,_,c1) >= T (_,_,c2) = c1 >= c2
  T (_,_,c1) <  T (_,_,c2) = c1 < c2
  T (_,_,c1) <= T (_,_,c2) = c1 <= c2
  first@(T (_,_,c1)) `max` second@(T (_,_,c2)) = if c1 == max c1 c2
                                                   then first
                                                   else second
  first@(T (_,_,c1)) `min` second@(T (_,_,c2)) = if c1 == min c1 c2
                                                   then first
                                                   else second

instance Show Tuple where
  showsPrec p (T x) = showsPrec p x
