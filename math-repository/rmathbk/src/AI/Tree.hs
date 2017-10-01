module AI.Tree where

import System.Random
import Data.List
--import Data.String


data Tree a b = Node a (Tree a b) (Tree a b) 
              | Leaf b 
              deriving (Show, Read)


genTree [] min max rand = Leaf []
genTree [(label, val)] min max rand = Leaf [(label, val)]
genTree pairs min max rand 
   | max == min = Leaf pairs
   | otherwise = Node (labcs, sval) (leftT ) (rightT )
      where (labels, values) = unzip pairs
            (sval, nrand) = randomR (min, max) rand
            (lessers, greaters)  = partition (\p -> (snd p) < sval) pairs
            leftT = genTree lessers min sval nrand
            rightT = genTree greaters sval max nrand
            labcs = calcLabcs leftT +++ (calcLabcs rightT)
            calcLabcs (Leaf pairs) = 
               let labels = map fst pairs -: sort 
                in zip (nub labels) (map length (group labels))
            calcLabcs (Node (labcounts, sval) _ _) = labcounts

g -: f = f g

xs +++ ys = let alls = sort (xs ++ ys)
                grouped = groupBy (\(a,_) (c,_) -> a==c) alls
             in map (\a -> foldl (\(l,n) (_,m)-> (l,n+m)) (head a) (tail a)) grouped



sgenTree [] min max rand = Leaf []
sgenTree [(label, val)] min max rand = Leaf [(label, val)]
sgenTree pairs min max rand 
   | max == min = Leaf pairs
   | otherwise = Node (sval) (leftT ) (rightT )
      where (labels, values) = unzip pairs
            (sval, nrand) = randomR (min, max) rand 
            (lessers, greaters)  = partition (\p -> (snd p) < [sval]) pairs
            leftT = sgenTree lessers min (pred sval) nrand
            rightT = sgenTree greaters sval max nrand
            --labcs = calcLabcs leftT +++ (calcLabcs rightT)
            --calcLabcs (Leaf pairs) = 
            --   let labels = map fst pairs -: sort 
            --    in zip (nub labels) (map length (group labels))
            --calcLabcs (Node (labcounts, sval) _ _) = labcounts


