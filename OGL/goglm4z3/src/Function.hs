{-# LANGUAGE FlexibleInstances #-}

module Function
   ( comp
   , numToFunc
   , funcToNum
   ) where

--import Data.Bits
--import Data.Word
--import Data.Word.Word24
import Data.Matrix

set = [0..2]
--twoBits w i = (testBit' w (i+1))*2+(testBit' w (i))
--   where testBit' a b = toNum' (testBit a b)
--         toNum' False = 0
--         toNum' True = 1
--
--comp f g h = foldl bitComp 0 [0,2..16] 
--   where bitComp p i = p + (fp i)*2^i
--         fp i = twoBits f ((twoBits g i)*4+(twoBits h i))

--listify w = foldl foo [] [0,2..22]
--   where foo acc cur = (twoBits w cur):acc
--
--myToNum w = listify w -: reverse -: (\x -> polySum x 3)
--
--myFromList l = reverse l -: (\x -> polySum x 4)
--
--myFromNum n = numToPoly n 3 -: myFromList

comp f g h = fromList 3 3 [f ! (i+1,j+1) | k <- set, l <- set, let i = g ! (k+1,l+1), let j = h !  (k+1,l+1)]

numToFunc n = ((numToPoly n 3)++(repeat 0)) -: fromList 3 3
funcToNum f = toList f -: (\x -> polySum x 3)

instance Ord (Matrix Int) where
   m1 `compare` m2 = funcToNum m1 `compare` (funcToNum m2)

polySum [] n = 0
polySum [a] n = a
polySum (x:xs) n = n*(polySum xs n) + x

numToPoly 0 b = []
numToPoly n b = (n `rem` b):(numToPoly ((n - (n `rem` b)) `div` b) b)

f -: g = g f
