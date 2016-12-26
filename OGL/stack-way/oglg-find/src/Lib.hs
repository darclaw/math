module Lib
    ( someFunc
      ,size
      ,set
      ,cmap
      ,cmapIO_ f
      ,binComp
      ,funToNum
      ,numToFunc
      ,treeComp
      ,printPretty
      ,pp
      ,ppTree
      ,ppTC
      ,ppN
      ,inEOGLG
      ,InOGLG
      ,checkInOGLG
      ,parFindOGLG
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Func = [(Int,Int,Int)] 

size = 3
set = [0..(size-1)]

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

--EComp f, f is ag or bg
--Comp (cur function) (tree of operator composed under) (CTree left arg) (CTree right arg)
--data CTree f = EComp f | Comp f (CTree f) (CTree f) (CTree f) deriving (Show)
data CTree f = EComp {func::f} | Comp {func::f , compF::(CTree f), leftArg::(CTree f), rightArg::(CTree f)} deriving (Show, Eq)

cmap:: (a->b) -> CTree a -> CTree b
cmap f (EComp f1) = EComp (f f1)
cmap f (Comp f1 ct lt rt) = Comp (f f1) (cmap f ct) (cmap f lt) (cmap f rt)

cmapIO_ f (EComp f1) = do
   putStrLn "EComp"
   f f1
cmapIO_ f (Comp f1 ct lt rt) = do
   putStrLn "func"
   f f1
   putStrLn "CompF"
   cmapIO_ f ct
   putStrLn "leftArg"
   cmapIO_ f lt
   putStrLn "rightArg"
   cmapIO_ f rt

extract (Just (a,b,c)) = c
extract Nothing = -1

infixl 3 -:
f -: g = g f


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

ppN = pp.numToFunc

printList xs = mapM_ (\a -> do
                          putStr ("" ++ numToString a)) xs

numToString n = [(['0'..'9']++['a'..'z']++['A'..'Z']++['ÿ'..]) !! n]

inEOGLG :: (Eq a, Num a) => [(a, a, a)] -> Bool
inEOGLG f = let mDiag = [(a,b,c) | (a,b,c)<-f, a==b]
                other = [(a,b,c) | (a,b,c)<-f, a/=b]
             in and ((map (\(_,_,c)-> let (_,_,c1)= (head other) in c==c1)) (tail other)) 
               && (length mDiag == length (nubBy (\(_,_,c1) (_,_,c2)->c1==c2) mDiag)) 
               && all (\(a,b,c) -> a/=c) mDiag
         

data InOGLG a b c= IsInOGLG a | TimedOut b | IsSubSet c deriving (Show)
isIsInOGLG (IsInOGLG _) = True
isIsInOGLG _ = False
isTimedOut (TimedOut _) = True
isTimedOut _ = False
isIsSubSet (IsSubSet _) = True
isIsSubSet _ = False
--funcs initialy contains op, ag, bg
--checkInOGLG :: (Eq t, Eq t1, Eq t2) => [[(t,t1,t2)]]-> [b]
--n is iterations to run for, i is iterations ran
--oglg is the set of functions known to be in oglg 
checkInOGLG funcs oglg i 0= TimedOut (funcs, i)--Left (Just funcs,-1)
checkInOGLG funcs oglg i n
  | any (`S.member` oglg) (S.fromList (map func funcs)) = IsInOGLG ((find (\t -> (func t) `S.member` oglg) funcs),i)
  | length funcs > 136 = TimedOut (funcs, i)-- Left (Just funcs,-2) -- prevents running to long
  | otherwise = trace ("n ="++(show n)++" length funcs = "++(show (length funcs))) $
   let fs = [(f,fl,fr) | f<-funcs, fl<-funcs, fr<-funcs] 
       -- nTrees = [treeComp a b c | (a,b,c) <- fs]
       pnTrees = parMap rseq (\(a,b,c) -> treeComp a b c) fs
    in let nfs = nubBy checkEqOrTrans (pnTrees)
        in --if any (inEOGLG) (map func nfs) then Right ([(find (\t -> inEOGLG (func t)) nfs)],n) else 
            if length funcs == (length nfs) then IsSubSet (funcs, i) else checkInOGLG nfs oglg (i+1) (n-1)

checkEqOrTrans t1 t2 
   | (func t1)==ag && (func t2)==bg = False
   | (func t1)==bg && (func t2)==ag = False
   | otherwise = (func t1) == (func t2) || (func t1) == (binComp (func t2) bg ag) 

findOGLG [] oglg d = []
findOGLG (f:fs) oglg d = --trace ("f ="++(show (funcToNum f))) $
   let encV = checkInOGLG [EComp f, agT, bgT] oglg 0 d
    in decide encV
   where decide (IsInOGLG (Just f,x)) = (IsInOGLG f,x):(findOGLG fs (f `S.insert` oglg ) d)
         decide (IsSubSet (funcs,x)) = (IsSubSet funcs, x):(findOGLG (fs\\(map func funcs)) oglg)
         decide (TimedOut (funcs, i)) = (TimedOut funcs, i):(findOGLG fs oglg d)

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
