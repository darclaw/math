-- ogl written in haskell

size = 3

compose2::(a->a->a)->(a-> a-> a) -> (a->a->a)-> a -> a-> a
compose2 f g h x y= f (g x y) (h x y)

compose :: (a->a)->(a->a-> a) -> a -> a ->a
compose f g x y = f (g x y)

print1 name f = name ++"\n"++ show (map f [0..size] )

--needs to show a 
--[[f 0 0,f 0 1,..],
--[f 1 0, f 1 1,..],
-- ...
-- ]
print2 name f = 
   let
      print2nonName i 
         | i<=0 = [ map (f 0) [0..size] ]
         | i>0  = map (f i) [0..size] : print2nonName (i-1) 
   in
      name ++"\n"++ map replaceCommasWithNewlines (show ( reverse $ print2nonName size ))

replaceCommasWithNewlines ']' = '\n'
replaceCommasWithNewlines c = c
   

op x y
   | a==b = (a-1) `mod` size
   | otherwise = 0
   where a = x `mod` size
         b = y `mod` size

rot x = op x x
 
aGate 0 x _ = x
aGate n x y = rot ( aGate (n-1) x y )

bGate 0 _ y = y
bGate n x y = rot ( bGate (n-1) x y )

fGate a b x y
   | d==0 = compose2 op (aGate c) (bGate d) x y
   | otherwise =  rot $ fGate a ((b-1) `mod` size) x y
   where d= b `mod` size
         c= a `mod` size

nGate n x y
   | m==0 = compose2 op (aGate 0) (aGate 1) x y
   | otherwise = rot $ (nGate (m+1) ) x y
   where m = n `mod` size

--need to fix
iso a b c x y
   | c'==0 = nGate 0 x y
   | c'/=(size-1) = 
      let
         subfunc = compose2 (fGate (-2-c) 0) (aGate a') (bGate b')
      in 
         compose2 op subfunc (nGate (c+1)) x y
   | otherwise = 
      let 
         subfunc = compose2 (fGate (c-1) 1) (aGate a') (bGate b') 
      in
         compose2 op subfunc (nGate 0) x y
   where a'=a`mod`size
         b'=b`mod`size
         c'=c`mod`size

