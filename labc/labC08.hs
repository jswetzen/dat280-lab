module Main where
import Data.Array.Repa as R

main :: IO ()
main = print "hii"

data State = State { buy :: (Int, Int) -- (value, position)
                   , sell :: (Int, Int)
                   , mini :: (Int, Int)
                   } deriving (Show)

-- Applying buySell should produce (1,5,10)
exampleProblem :: Array U DIM1 Int
exampleProblem = fromListUnboxed (Z :. (8::Int)) [0,0,2,9,8,10,1,10]

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
-- Try One --
-------------

interleave :: [Int] -> [Int] -> [Int]
interleave []     xs = xs
interleave (x:xs) ys = x : interleave ys xs

evens :: [Int] -> [Int]
evens [] = []
evens [_] = []
evens (_:x:xs) = x : evens xs

odds :: [Int] -> [Int]
odds [] = []
odds [x] = [x]
odds (x:_:xs) = x : odds xs

scanOp :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
--scanOp op ident as = ident:scanOp' op ident as
scanOp = scanOp'

scanOp' :: (Int -> Int -> Int) -> Int -> [Int] -> [Int]
scanOp' _ ident [] = [ident]
scanOp' op ident as = let
    e = evens as
    o = odds as
    s = scanOp' op ident $ Prelude.zipWith op e o
  in interleave s $ Prelude.zipWith op s e
