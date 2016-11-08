import Data.List

size = 2

f2funcs = [ [[d,c],[b,a]] | a <- [0,1], b <- [0,1], c<-[0,1], d<-[0,1] ]

--funcToNum f = sum [(f a b)*size^(a*size + b) | a <- [0..size-1], b<- [0..size-1] ] 
--(f 0 0) + (f 0 1)*size + (f 1 0)*size^2 + (f 1 1)*size^3
funcToNum f = fst $ [f a b | a<-[0..size-1], b<-[0..size-1]] -: foldl (\(acc,pow) x -> (acc+x*size^pow,pow+1)) (0,0)

numToFunc n = (\x y -> ((take (size^2) (digs n))) !! (x*size+y))

--digs :: Int x => x -> [x]
digs 0 = [0,0..]
digs x = (x `mod` (size)) : (digs (x `div` size))

f -: g = g f


composeNums op f g x y= (numToFunc op) ((numToFunc f) x y) ((numToFunc g) x y)

composeCube =  let u = [0 .. size^(size^2)-1] in [(z,x,y, num) | z<-u, (x,y,num)<-(composeSquare z)]

composeSquare n = let u = [0..size^(size^2)-1] in [(x,y, funcToNum (composeNums n x y)) | x<-u, y<-u]

printComposeSquare n = do
   let xs = composeSquare n
   putStr (numToString n)
   putStr " |"
   printList [0..size^4-1]
   putStrLn ""
   putStrLn (take ((size^4)+3) (repeat  '-'))
   let groupedSquare = groupBy (\(a,_,_) (b,_,_) -> a==b) xs
   mapM_ (\(a,list) -> do
       putStr (numToString a) 
       putStr " |"
       printList (map (\(_,_,num) -> num) list)
       putStrLn "") 
       ( zip [0..size^4-1] groupedSquare)
   putStrLn ""

printList xs = mapM_ (\a -> do
                          putStr ("" ++ numToString a)) xs
                          --putStrLn "") xs

numToString n = [(['0'..'9']++['a'..'z']++['A'..'Z']++['ÿ'..]) !! n]
