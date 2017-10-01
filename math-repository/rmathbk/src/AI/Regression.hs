module AI.Regression where

import Data.List

data Regres a b = (Regres a b) :+ (Regres a b)
                 | FGauss a a b
                 | Const b
                 deriving (Show, Read)

gauss sigma means vars = sqrt(2*pi*sigma^2) -\- exp(-(2*sigma^2)-\-(sum (zipWith (\a b -> (a-b)^2) means vars)))
   where b -\- a = a/b

fgauss σs μs scal vars = scal*(exp (-(1/2)*(sum (map (\(σ,μ,v) -> (1/σ)*(v-μ)^2) trips))))
   where trips = zip3 σs μs vars

toFunc (Const a) vars = a
toFunc (FGauss sig mu scal)  vars = fgauss sig mu scal vars
toFunc (a :+ b) vars = (toFunc (a) vars) + (toFunc (b) vars)

basefit funcdata = (FGauss σ σ μ) 
   where (ys, xss) = unzip funcdata
         μ = (sum ys) / (fromIntegral (length ys))
         σ = sqrt $ sum $ map (\v -> (μ-v)^2) ys

--rgauss:: Num a => [(a,[a])] -> a -> Regres [a] a
rgauss [] d= Const 0
rgauss [x] d= Const 0
rgauss fvals 0 = Const 0
rgauss fvals d = (FGauss σs μs scal) :+ rest --(rgauss after (d-1))
   where (ys, xss) = unzip fvals
         scal = mean ys
         txss = transpose xss
         --μs = map mean txss
         --σs = map (std) txss
         (μs, σs) = unzip (map stdAndMean txss)
         after = zip nys xss
         nys = map (\(y,xs) -> y - (fgauss σs μs scal xs)) fvals
         sfvals = split μs fvals
         rest = foldl (:+) (Const 0) (map (\fs -> rgauss fs (d-1)) sfvals)

data Tree a b = Node b (Tree a b) (Tree a b)
              | End a
              deriving (Read, Show)

--split p [] = []
--split [] fvals = []
--split:: [a] -> [(a,[a], [a])] -> [(a,[a],[a])]
split' [] fvals = End (map (\(v,nxs,xs) -> (v,nxs)) fvals)
split' (p:ps) fvals = Node p (split' ps lessers) (split' ps greaters)
   where (lessers',greaters') = partition (\(y, nxs, (x:xs)) -> x<=p) fvals
         lessers = map (\(y,nxs,(x:xs)) -> (y,x:nxs,xs)) lessers'
         greaters = map (\(y,nxs,(x:xs)) -> (y,x:nxs,xs)) greaters'

split p fvals = unfoldTree $ split' p (map (\(v,xs) -> (v,[],xs)) fvals)

unfoldTree (End a) = [a]
unfoldTree (Node b x y) = (unfoldTree x)++(unfoldTree y)

std::Floating f => f -> [f] -> f
std μ xs = sqrt $ sum $ map (\v -> (μ-v)^2) xs
mean xs = (sum xs)/(fromIntegral (length xs))

stdAndMean xs = (mean xs, std (mean xs) xs)
