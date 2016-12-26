import Lib

main = do
   let x = map (funcToNum) (parFindOGLG (allFuncs) eoglg (-1))
   putStrLn (show x)

oglgl = S.toList eoglg

nelem a l = not (a `elem` l)

hypForOGLG :: Func -> (Func,Bool)
hypForOGLG func@[ (0,0,a),(0,1,b),(0,2,c) ,(1,0,d),(1,1,e),(1,2,f) ,(2,0,g),(2,1,h),(2,2,j)] = 
   let diag = [a,e,j] 
       rows = [[a,b,c],[d,e,f],[g,h,j]]
       cols = [[a,d,g],[b,e,h],[c,f,j]]
       drc = diag:(rows++cols)
       eachOne = [a,b,c,d,e,f,g,h,j]
       --
       neqId = (/=[0..size])
       nAllEq i= not (all (==(head i)) i)
       nAnyEq i= length (nub i) == length i
       --
    in (
          func
          ,and (map neqId drc) 
          && nAllEq diag 
          && all (`elem` eachOne) set 
          && (any (`elem`(permutations set)) [[a,b,c]| a<-(rows!!0), b<-(rows!!1),c<-(rows!!2)] 
             || any (\[a,b,c] -> a/=0&&b/=1&&c/=2) rows)
          -- && if not (nAllEq (cols) || (nAllEq rows)) then True
             --else diag `elem` (permutations set)
       )
hypForOGLG f = (f,False)

hypMatches fs = map fst (filter snd (map hypForOGLG fs))

hypForOGLG' :: Func -> (Func,Bool)
hypForOGLG' func@[ (0,0,a),(0,1,b),(0,2,c) ,(1,0,d),(1,1,e),(1,2,f) ,(2,0,g),(2,1,h),(2,2,j)] = 
   let diag = [a,e,j] 
       s1diag = [b,f,g]
       s2diag = [c,d,h]
       rows = [[a,b,c],[d,e,f],[g,h,j]]
       cols = [[a,d,g],[b,e,h],[c,f,j]]
       drc = diag:(rows++cols)
       eachOne = [a,b,c,d,e,f,g,h,j]
       --
       neqId = (/=[0..(size-1)])
       nAllEq i= not (all (==(head i)) i)
       nAnyEq i= length (nub i) == length i
       --
    in (
          func
          ,and (map neqId drc) 
          && nAllEq diag 
          && all (`elem` eachOne) set 
          && (any (`elem`(permutations set)) [[a,b,c]| a<-(rows!!0), b<-(rows!!1),c<-(rows!!2)] 
             || any (\[a,b,c] -> a/=0&&b/=1&&c/=2) rows)
          && (any (`elem`(permutations set)) [[a,b,c]| a<-(cols!!0), b<-(cols!!1),c<-(cols!!2)] 
             || any (\[a,b,c] -> a/=0&&b/=1&&c/=2) cols)
          -- && if not (nAllEq (eachOne \\ diag)) then diag `elem` (permutations set) else True
          -- && (nAnyEq rows || nAnyEq cols)
       )
hypForOGLG' f = (f,False)

hypMatches' fs = map fst (filter snd (map hypForOGLG' fs))
