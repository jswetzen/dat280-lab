module Main where
import Data.Array.Repa as R

main :: IO ()
main = print "hii"

-- Applying buySell should produce (1,5,10)
exampleProblem :: Array U DIM1 Int
exampleProblem = fromListUnboxed (Z :. (8::Int)) [0,0,2,9,8,10,1,10]

buySellSeq :: Array U DIM1 Int -> (Int, Int, Int)
buySellSeq arr = buySellSeq' (x, 0) (x, 0) (x, 0) 1 lst
  where (x:lst) = toList arr

-- (buy value, position) -> (sell val, pos) -> (minimum val, pos) -> [list] -> (bPos, sPos, profit)
buySellSeq' :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> [Int] -> (Int, Int, Int)
buySellSeq' (bVal, bPos) (sVal, sPos) (_, _) _ [] = (bPos, sPos, sVal - bVal)
buySellSeq' (bVal, bPos) (sVal, sPos) (mVal, mPos) pos (x:xs)
  | x > sVal = buySellSeq' (bVal, bPos) (x, pos) (mVal, mPos) (pos + 1) xs -- Update sVal
  | (x-mVal) > (sVal-bVal) = buySellSeq' (mVal, mPos) (x, pos) (mVal, mPos) (pos+1) xs -- Update buy/sell
  | x < mVal = buySellSeq' (bVal, bPos) (sVal, sPos) (x, pos) (pos+1) xs -- Update mVal

