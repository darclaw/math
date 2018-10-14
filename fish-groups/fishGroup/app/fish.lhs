%options ghci -fglasgow-exts
\documentclass{article}
%include polycode.fmt


\usepackage{amsthm}
\usepackage{amssymb}
\usepackage{amsmath}

\usepackage{listings}
\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small\ttfamily}}{}

\long\def\ignore#1{}


%\DeclareMathOperator{\mod}{mod}
\DeclareMathOperator{\ord}{ord}
\begin{document}


\ignore{
\begin{code}
import Math.Algebra.Group.PermutationGroup
import Math.Core.Utils ((^-))
import Data.List

f .> g = g f
\end{code}
}

Let 

\begin{code}
fish :: Int -> [Permutation Int]
fish n = [r1 n, r2 n]
r1 :: Int -> Permutation Int
r1 n = p [[1..n]]
r2 :: Int -> Permutation Int
r2 n = p ([[2,(2*n-2),4]++[(2*n-3),(2*n-4)..(n+1)]])
\end{code}

Note that this is a subset of $S_{2*n-2}$. 

Let 

\begin{code}
m n k | k<0 = let pk = -k in ((r1 n)^-pk) * ((r2 n)^-pk) * (r1 n)^pk * (r2 n)^pk
      | k>=0 = (r1 n)^k*(r2 n)^k * ((r1 n)^-k)* ((r2 n)^-k)
\end{code}

Then we find that for any $n\geq 5$, we find that $m\ n\ (\pm 2)$ permutes 4 elements and has order 2. Further, for any $n\geq 6$,  $m\ n\ (\pm 1,3)$ permutes 6 elements and has order 3. It appears that for any $n\geq 4$, we have that $m\ n\ k$ has order 3 and permutes 6 elements if $k \neq \pm 2 \mod n$.

We find that the order of $fish\ 4 =24$ and $\ord(fish\ 5) = 20160=4*7!$. 

For Fish 5, we find that $n_7$ is gotten by



\begin{code}
a `dv` b = (b `mod` a) == 0
n7 = [(a,b,c,d) | a<-[0..1],b<-[0..1],c<-[0..2],d<-[0..6]
               , let np = 7^a*5^b*3^c*2^d
               , np `dv` (5*3^2*2^6)
               , np `mod` 7 == 1]
n5 = [(a,b,c,d) | a<-[0..1],b<-[0..1],c<-[0..2],d<-[0..6] 
      ,let np = 7^a*5^b*3^c*2^d , ((7*3^2*2^6) `mod` np) == 0, np `mod` 5 == 1]
n3 = [(a,b,c,d) | a<-[0..1],b<-[0..1],c<-[0..2],d<-[0..6] 
      , let np = 7^a*5^b*3^c*2^d , ((7*5*2^6) `mod` np) == 0, np `mod` 3 == 1]
n2 = [(a,b,c,d) | a<-[0..1],b<-[0..1],c<-[0..2],d<-[0..6] 
      , let np = 7^a*5^b*3^c*2^d , ((7*5*3^2) `mod` np) == 0, np `mod` 2 == 1]
\end{code}


which gives |n7| to be  \eval{ map (\(a,b,c,d)-> 7^a*5^b*3^c*2^d) [(a,b,c,d) | a<-[0..1],b<-[0..1],c<-[0..2],d<-[0..6] , let np = 7^a*5^b*3^c*2^d , ((5*3^2*2^6) `mod` np) == 0, np `mod` 7 == 1] }, %\)
|n5| = \eval{ [7^a*5^b*3^c*2^d | a<-[0..1],b<-[0..1],c<-[0..2],d<-[0..6] , let np = 7^a*5^b*3^c*2^d , ((7*3^2*2^6) `mod` np) == 0, np `mod` 5 == 1] },
|n3| = \eval{[7^a*5^b*3^c*2^d | a<-[0..1],b<-[0..1],c<-[0..2],d<-[0..6] , let np = 7^a*5^b*3^c*2^d , ((7*5*2^6) `mod` np) == 0, np `mod` 3 == 1]},
|n2| = \eval{[7^a*5^b*3^c*2^d | a<-[0..1],b<-[0..1],c<-[0..2],d<-[0..6] , let np = 7^a*5^b*3^c*2^d , ((7*5*3^2) `mod` np) == 0, np `mod` 2 == 1]}

\end{document}
