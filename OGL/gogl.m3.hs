import Data.List
import Control.Monad
import qualified Data.Set as S
import Data.Either
import Debug.Trace
import Control.Parallel
import Control.Concurrent

size = 3
set = [0..(size-1)]

op :: [(Int,Int,Int)]
op = [ (0,0,2),(0,1,0),(0,2,1)
      ,(1,0,1),(1,1,0),(1,2,2)
      ,(2,0,2),(2,1,2),(2,2,0)]
--opT = Comp op opT agT bgT
opT = EComp op

ag :: [(Int,Int,Int)]
ag = [ (0,0,0),(0,1,0),(0,2,0)
      ,(1,0,1),(1,1,1),(1,2,1)
      ,(2,0,2),(2,1,2),(2,2,2)]
agT = EComp ag

bg :: [(Int,Int,Int)]
bg = [ (0,0,0),(0,1,1),(0,2,2)
      ,(1,0,0),(1,1,1),(1,2,2)
      ,(2,0,0),(2,1,1),(2,2,2)]
bgT = EComp bg

type Func = [(Int,Int,Int)] 

--EComp f, f is ag or bg
--Comp (cur function) (tree of operator composed under) (CTree left arg) (CTree right arg)
--data CTree f = EComp f | Comp f (CTree f) (CTree f) (CTree f) deriving (Show)
data CTree f = EComp {func::f} | Comp {func::f , compF::(CTree f), leftArg::(CTree f), rightArg::(CTree f)} deriving (Show, Eq)

cmap:: (a->b) -> CTree a -> CTree b
cmap f (EComp f1) = EComp (f f1)
cmap f (Comp f1 ct lt rt) = Comp (f f1) (cmap f ct) (cmap f lt) (cmap f rt)

extract (Just (a,b,c)) = c
extract Nothing = -1


binComp :: Func -> Func -> Func -> Func
--binComp uF f1 f2 = do
--   (a1,b1,c1) <- f1
--   (a2,b2,c2) <- f2
--   guard (a1 == a2 && b1 == b2)
--   let v = extract (find (\(a,b,_) -> (a==c1 && b==c2)) uF)
--   return (a1,b1,v)
binComp uF f1 f2 = do
   (a1,b1,c1) <- f1
   let (_,_,c2) = f2 !! (a1*size+b1) --find (\(a,b,_) -> a==a1 && b==b1) f2
   let (_,_,v) = uF !! (c1*size+c2) -- find (\(a,b,_) -> (a==c1 && b==c2)) uF
   return (a1,b1,v)

funcToNum f = foldl (\acc (a,b,c) -> acc+c*size^(a*size+b)) 0 f

numToFunc n = [(a,b,numToFunction n a b) | a<-set, b<-set]
numToFunction n = (\x y -> ((take (size^2) (digs n))) !! (x*size+y))
digs 0 = [0,0..]
digs x = (x `mod` (size)) : (digs (x `div` size))

listToFunc l= numToFunc $ sum (zipWith (\x y-> x*3^y) l [0..]) 

treeComp uT@(EComp uF) t1@(EComp f1) t2@(EComp f2) = Comp (binComp uF f1 f2) uT t1 t2
treeComp uT@(EComp uF) t1@(EComp f1) t2@(Comp f2 _ _ _) = Comp (binComp uF f1 f2) uT t1 t2
treeComp uT@(EComp uF) t1@(Comp f1 _ _ _) t2@(EComp f2) = Comp (binComp uF f1 f2) uT t1 t2
treeComp uT@(EComp uF) t1@(Comp f1 _ _ _) t2@(Comp f2 _ _ _) = Comp (binComp uF f1 f2) uT t1 t2
treeComp uT@(Comp uF _ _ _) t1@(EComp f1) t2@(EComp f2) = Comp (binComp uF f1 f2) uT t1 t2
treeComp uT@(Comp uF _ _ _) t1@(EComp f1) t2@(Comp f2 _ _ _) = Comp (binComp uF f1 f2) uT t1 t2
treeComp uT@(Comp uF _ _ _) t1@(Comp f1 _ _ _) t2@(EComp f2) = Comp (binComp uF f1 f2) uT t1 t2
treeComp uT@(Comp uF _ _ _) t1@(Comp f1 _ _ _) t2@(Comp f2 _ _ _) = Comp (binComp uF f1 f2) uT t1 t2

trans f = binComp f bg ag

treeTrans f = treeComp f bgT agT

rot 0 f1 = f1
rot 1 f1 = binComp op f1 f1
rot n f1 = rot 1 (rot (n-1) f1)

treeRot 0 t = t
treeRot 1 t = treeComp opT t t
treeRot n t = treeRot 1 (treeRot (n-1) t)

fng a b = rot b (binComp op (rot a ag) (rot a bg))

treeFng a b = treeRot b (treeComp opT (treeRot a agT) (treeRot a bgT))


printPretty n f = do
   let xs = f
   putStrLn ("N: "++(show (funcToNum f)))
   putStr (numToString n)
   putStr " |"
   printList [0..(n-1)]
   putStrLn ""
   putStrLn (take (n+3) (repeat  '-'))
   let groupedSquare = groupBy (\(a,_,_) (b,_,_) -> a==b) xs
   mapM_ (\(a,list) -> do
       putStr (numToString a) 
       putStr " |"
       printList (map (\(_,_,num) -> num) list)
       putStrLn "") 
       ( zip [0..size^4-1] groupedSquare)
   putStrLn ""

pp f = printPretty size f
ppTree t = pp (func t)
ppTC t1 t2 t3 = ppTree (treeComp t1 t2 t3)

printList xs = mapM_ (\a -> do
                          putStr ("" ++ numToString a)) xs

numToString n = [(['0'..'9']++['a'..'z']++['A'..'Z']++['ÿ'..]) !! n]

--for 
fop1 = [ (0,0,2),(0,1,1),(0,2,1)
      ,(1,0,0),(1,1,0),(1,2,1)
      ,(2,0,0),(2,1,0),(2,2,1)]
--ct = treeComp opT (treeFng 0 0) (treeFng 2 1) 
--dt = (treeComp ct (treeFng 2 0) (treeFng 1 0))
--et = (treeComp dt (treeFng 1 1) (treeFng 0 1))
--ft = (treeComp et (agT) (treeRot 1 bgT))
--gt = (treeComp opT (ft) (treeFng 2 1))
--n1t = (treeComp gt (gt) (treeRot 1 gt))
--
--tN 0 = treeRot 1 n1t
--tN 1 = n1t
--tN 2 = treeRot 2 n1t
--ht = (treeComp op (treeFng 1 2) op) -- in OGLG

--for
fop2 = [ (0,0,2),(0,1,1),(0,2,0)
      ,(1,0,1),(1,1,0),(1,2,2)
      ,(2,0,2),(2,1,0),(2,2,1)]
--ct = treeComp opT (treeTrans opT) (opT)
--dt = treeComp opT ct (treeTrans ct)
--et = treeComp (treeTrans ct) (treeTrans ct) (treeTrans ct)
--wantT = treeComp opT dt (treeRot 2 et)  -- in OGLG

fop3 = numToFunc 5527
fop4 = numToFunc 6049


inEOGLG :: (Eq a, Num a) => [(a, a, a)] -> Bool
--inEOGLG f = inEOGLGhelper f (extract (find (\(a,b,_) -> (a/=b)) f))
--   where inEOGLGhelper [] c = True
--         inEOGLGhelper ((a,b,v):fs) c =  
--            if a == b then (if a==v then False else inEOGLGhelper fs c) else
--               (if v /= c then False else inEOGLGhelper fs c)
inEOGLG f = let mDiag = [(a,b,c) | (a,b,c)<-f, a==b]
                other = [(a,b,c) | (a,b,c)<-f, a/=b]
             in and ((map (\(_,_,c)-> let (_,_,c1)= (head other) in c==c1)) (tail other)) 
               && (length mDiag == length (nubBy (\(_,_,c1) (_,_,c2)->c1==c2) mDiag)) 
               && all (\(a,b,c) -> a/=c) mDiag
         
--create every function in F_2(set)
--n^2 positions, n things for each pos
--n^(n^2) total functions
--

allFuncs = [[ (0,0,a),(0,1,b),(0,2,c)
           ,(1,0,d),(1,1,e),(1,2,f)
            ,(2,0,g),(2,1,h),(2,2,j)] |j<-set,h<-set,g<-set,f<-set,e<-set,d<-set,c<-set,b<-set,a<-set ]

eoglgpre = (filter inEOGLG allFuncs)++[fop1,fop2,fop3,fop4]
eoglg = S.fromList (eoglgpre++(map trans eoglgpre))

--funcs initialy contains op, ag, bg
--checkInOGLG :: (Eq t, Eq t1, Eq t2) => [[(t,t1,t2)]]-> [b]
--n is iterations to run for, i is iterations ran
--oglg is the set of functions known to be in oglg 
checkInOGLG funcs oglg i 0= Left (Nothing,0)
checkInOGLG funcs oglg i n
  | any (`S.member` oglg) (S.fromList (map func funcs)) = Right ([(find (\t -> (func t) `S.member` oglg) funcs)],n)
  | length funcs > 15 = Left (Nothing,0) -- prevents running to long
  | otherwise = --trace ("n ="++(show n)++" length funcs = "++(show (length funcs))) $
   let fs = [(f,fl,fr) | f<-funcs, fl<-funcs, fr<-funcs] 
    in let nfs = nubBy checkEqOrTrans [treeComp a b c | (a,b,c)<-fs]
        in --if any (inEOGLG) (map func nfs) then Right ([(find (\t -> inEOGLG (func t)) nfs)],n) else 
            -- (if all (`elem` (map func funcs)) (map func nfs) then (Nothing,n) else checkInOGLG nfs (n+1))
            -- (if all (`S.member` (S.fromList (map func funcs))) (map func nfs) then Left (Just funcs,n) else checkInOGLG nfs oglg (n+1))
            -- (if (S.fromList (map func funcs)) == (S.fromList (map func nfs)) then Left (Just funcs, n) else checkInOGLG nfs oglg (n+1))
            if length funcs == (length nfs) then Left (Just funcs, n) else checkInOGLG nfs oglg (i+1) (n-1)

--checkInOGLG'' funcs oglg n 
--  | any (`S.member` oglg) (S.fromList (map func funcs)) = Right ([(find (\t -> (func t) `S.member` oglg) funcs)],n)
--  | otherwise = trace ("n ="++(show n)++" length funcs = "++(show (length funcs))) $
--   let fs = [(f,fl,fr) | f<-funcs, fl<-funcs, fr<-funcs] 
--    in let nfs = nubBy checkEqOrTrans [treeComp a b c | (a,b,c)<-fs]
--        in if any (`S.member` eoglg) (map func nfs) then Right ([(find (\t -> inEOGLG (func t)) nfs)],n) else 
--            -- (if all (`elem` (map func funcs)) (map func nfs) then (Nothing,n) else checkInOGLG nfs (n+1))
--            -- (if all (`S.member` (S.fromList (map func funcs))) (map func nfs) then Left (Just funcs,n) else checkInOGLG nfs oglg (n+1))
--            (if (map func funcs) == (map func nfs) then Left (Just funcs, n) else checkInOGLG nfs oglg (n+1))
--
--checkInOGLG' funcs oglg n 
--  | any (`S.member` oglg) (S.fromList (map func funcs)) = Right ([(find (\t -> (func t) `S.member` oglg) funcs)],n)
--  | any (`S.member` eoglg) (S.fromList (map func funcs)) = Right ([(find (\t -> (func t) `S.member` eoglg) funcs)],n)
--  | otherwise = trace ("n ="++(show n)++" length funcs = "++(show (length funcs))) $
--     let fts = [(f,fl,fr) | f<-fs, fl<-fs, fr<-fs]
--         fs = if n==0 then sortDropTree funcs else funcs
--      in let nfs = {-sortDropTree-} nubBy checkEqOrTrans [treeComp a b c | (a,b,c) <- fts]
--             nfsSet = S.fromList (map func nfs)
--          in if (map func nfs) == (map func fs) then Left (Just fs, n) else checkInOGLG' nfs oglg (n+1)


checkEqOrTrans t1 t2 
   | (func t1)==ag && (func t2)==bg = False
   | (func t1)==bg && (func t2)==ag = False
   | otherwise = (func t1) == (func t2) || (func t1) == (binComp (func t2) bg ag) 

sortDropTree [] = []
sortDropTree [t] = [t]
sortDropTree (t:ts) = less++[t]++greater
   where less = filter (\t1 -> (func t1)<(func t)) ts
         greater = filter (\t1 -> (func t1)>(func t)) ts

singleOGLG [] oglg = []
singleOGLG (f:fs) oglg = 
   let encV = checkInOGLG [EComp f] oglg 0 (-1)
    in decide encV
   where decide (Right ([Just x],_)) = (f,x):(singleOGLG fs (f `S.insert` oglg ))
         decide (Left _) = singleOGLG fs oglg

doubleOGLG [] oglg = []
doubleOGLG ((f,g):fs) oglg = 
   let encV = checkInOGLG [EComp f, EComp g] oglg 0 (-1)
    in decide encV
   where decide (Right ([Just x],_)) = (f,g,x):(doubleOGLG fs (f `S.insert` oglg )) --need to do something more than insert f oglg
         decide (Left _) = doubleOGLG fs oglg

tripleOGLG [] oglg = []
tripleOGLG ((f,g,h):fs) oglg = 
   let encV = checkInOGLG [EComp f, EComp g, EComp h] oglg 0 (-1)
    in decide encV
   where decide (Right ([Just x],_)) = (f,g,h,x):(tripleOGLG fs (f `S.insert` oglg ))
         decide (Left _) = tripleOGLG fs oglg

findOGLG [] oglg d = []
findOGLG (f:fs) oglg d = --trace ("f ="++(show (funcToNum f))) $
   let encV = checkInOGLG [EComp f, agT, bgT] oglg 0 d
    in decide encV
   where decide (Right ([Just x],_)) = (f,x):(findOGLG fs (f `S.insert` oglg ) d)
         decide (Left _) = findOGLG fs oglg d

parFindOGLG [] oglg d= []
parFindOGLG funcs@(f:fs) oglg d = 

   let j = 120 --number of concurrent 
    in 
   let (jFs,restFs) = splitAt j funcs
       in
   let rjFs = map (\f -> findOGLG [f] oglg d) jFs
       in
   let x = foldl par (head rjFs) (rjFs) 
       in
   let y = (fmap fst (mconcat rjFs))
       in
   let noglg = oglg `S.union` (S.fromList y)
       in
   (y++(parFindOGLG restFs noglg d))
   


divides m n = rem n m == 0

--primeFact::Int->[Int]
factors n = foldl (\acc x -> if x `divides` n then x:acc else acc) [] [1..n]

primeFact n = foldl (\acc x -> if length (factors x) == 2 then x:acc else acc) [] (factors n)

main = do
   let x = map (funcToNum) (parFindOGLG (allFuncs) eoglg (-1))
   putStrLn (show x)
