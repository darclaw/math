module Main where

import Lib
import Function
import Data.Either
import qualified Data.Set as S
import Data.Bits
--import Data.Word
import Data.Word.Word24
import Data.List

import Data.Matrix

main :: IO ()
main = do
   let comps = mapZ (\x -> subSetSize [numToFunc x,ag,bg] 2) [0..(3^9-1)]
   let comps4 = filter (\(_,b) -> b==4) comps
   putStrLn "Num of Funcs with |fspan(f,ag,bg)|=4"
   putStrLn (show comps4)
   putStrLn $ (show.length) comps4

--ag = myFromNum 15897
ag = matrix 3 3 (\(a,_) -> a-1)

--bg = myFromNum 19305
bg = matrix 3 3 (\(_,b) -> b-1)

subSetSize funcs n 
  | n==0 = 0
  | otherwise = let compFs = (S.toList . S.fromList) [comp f fl fr | f<-funcs, fl<-funcs, fr<-funcs]
                 in if length (compFs) == (length funcs) then (length funcs) else subSetSize (compFs) (n-1)


f -: g = g f

mapZ f l = zip l (map f l)


