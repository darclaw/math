{-# LANGUAGE OverloadedStrings #-}
module Handlers.AddHandler where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import qualified Data.ByteString.Char8 as B8
import Snap.Extras.JSON
--import Data.List
import qualified Data.Map as Map
import AI.Tree
import AI.Regression
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Matrix as Mat
import Database.MongoDB
import qualified Data.List as L

add :: Snap ()
add = do
   --rdata <- getJSON :: Snap (Either String String)
   rdata <- getParams 
   let vals = fromMaybe [] (Map.lookup (B8.pack "vals[]") rdata) --getParam "vals[]"
   let val_ts = fromMaybe [] (Map.lookup (B8.pack "val_ts[]") rdata) --getParam "vals[]"
   let keys = fromMaybe [] (Map.lookup (B8.pack "keys[]") rdata) --getParam "vals[]"

   let arbdata = zip3 keys val_ts vals

   let adjMatSize = (readB8.head) $ fromMaybe ["0"] $ Map.lookup (B8.pack "adjMatSize") rdata
   let adjMat' = [Map.lookup (B8.pack ("adjMat["++(show n)++"][]")) rdata | n <- [0..(adjMatSize-1)]]

   let adjMat'' = map (fromMaybe (take adjMatSize (repeat "0"))) adjMat'
   let adjMat'3 = map (map readB8) adjMat'' ::[[Integer]]
   let adjMat = Mat.fromLists adjMat'3
   let adjMatCharPoly = charPoly (fmap fromInteger adjMat)

   let pipe = connect $ host "127.0.0.1"
   liftIO $ 
      pipe >>= (\p -> access p master "rmath" 
                        (insert "graph"
                           [ "adjMat" =: show adjMat
                           , "arbdata" =: show arbdata
                           , "nodeNum" =: adjMatSize
                           ]
                        )
               )
   
   writeStream (fullMatPath (Mat.nrows adjMat) adjMat) --((read (B8.unpack adjMat)::[Integer])!0)

--addData arbd adjMat = insert "graph" ["adjMat" =: show adjMat, "arbd" =: show arbd]


writeStream x = writeBS ((B8.pack.show) x)
showB8 x = B8.pack (show x)
readB8 x = read (B8.unpack x)

fullMatPath 0 m = Mat.identity (Mat.nrows m)
fullMatPath n m = let pm = fullMatPath (n-1) m in myOr (pm) (pm*m)
                      where myOr a b = Mat.elementwise (or') a b
                            or' a b = if (a>0) ||  (b>0) then 1 else 0


data Poly a b = Var b 
              | PConst a
              | (Poly a b) :*: (Poly a b)
              | (Poly a b) :+: (Poly a b)
              | (Poly a b) :^: a
              deriving (Show, Read, Eq)

instance Num a => Num (Poly a b) where
   fromInteger x = PConst (fromInteger x)
   (+) = (:+:)
   (*) = (:*:)
   --abs (Abs (Abs x)) = Abs x
   abs x = x   -- breaks rules
   signum x = PConst 1  -- breaks rules
   negate x = (PConst (-1)) :*: x

charPoly m = Mat.detLaplace (m - (Mat.scaleMatrix (Var "x") (Mat.identity (Mat.nrows m))))

simplify (Var b) = Var b
simplify (PConst a) = PConst a

simplify ((PConst a):+:(PConst b)) = PConst (a+b)
simplify ((PConst a):*:(PConst b)) = PConst (a*b)

simplify ((PConst a):+:((PConst b):+:c)) = (PConst (a+b)):+:(simplify c)
simplify ((c:+:(PConst a)):+:(PConst b)) = (simplify c):+:(PConst (a+b))
simplify ((PConst a):*:((PConst b):*:c)) = (PConst (a*b)):*:(simplify c)
simplify ((c:*:(PConst a)):*:(PConst b)) = (simplify c):*:(PConst (a*b))

simplify (x:*:(PConst y))
  | (fromIntegral y) == (fromIntegral 0) = PConst (fromIntegral 0)
  | (fromIntegral y) == (fromIntegral 1) = simplify x
  | otherwise = (simplify x):*:(PConst y)
simplify ((PConst y):*:x)
  | (fromIntegral y) == (fromIntegral 0) = PConst (fromIntegral 0)
  | (fromIntegral y) == (fromIntegral 1) = simplify x
  | otherwise = (PConst y):*:(simplify x)

simplify (x:+:(PConst y))
  | (fromIntegral y) == (fromIntegral 0) = simplify x
  | otherwise = (simplify x):+:(PConst y)
simplify ((PConst y):+:x)
  | (fromIntegral y) == (fromIntegral 0) = simplify x
  | otherwise = (PConst y):+:(simplify x)

simplify ((x:+:y):*:z) = ((simplify x):*:(simplify z)):+:((simplify y):*:(simplify z))
simplify (x:*:(y:+:z)) = ((simplify x):*:(simplify z)):+:((simplify y):*:(simplify z))

simplify ((x:^:y):*:z) 
  | x==z = (simplify x):^:(y+(fromIntegral 1))
  | otherwise = ((simplify x):^:y):*:(simplify z)
simplify (z:*:(x:^:y)) 
  | x==z = (simplify x):^:(y+(fromIntegral 1))
  | otherwise = (simplify z):*:((simplify x):^:y)

simplify (x:+:y)
  | x==y = (PConst (fromInteger 2)):*:(simplify x)
  | otherwise = ((simplify x):+:(simplify y))
simplify (x:*:y)
  | x==y = (simplify x):^:(fromInteger 2)
  | otherwise = ((simplify x):*:(simplify y))

simplify (x:^:y) = (simplify x):^:y
--simplify x = x

realSimp p = if p==(simplify p) then p else realSimp (simplify p)


prettyShow (Var b) = show b
prettyShow (PConst a) = show a
prettyShow (a:+:b) = "("++(prettyShow a)++"+"++(prettyShow b)++")"
prettyShow (a:*:b) = "("++(prettyShow a)++"*"++(prettyShow b)++")"
prettyShow (a:^:b) = (prettyShow a)++"^"++(show b)

polyToList (PConst a) = [((PConst a), (fromInteger 0))]
polyToList (Var b) = [((Var b), (fromInteger 1))]
polyToList exp@((PConst a):*:(Var b)) = [(exp, (fromInteger 1))]
polyToList (((PConst a):*:b:^:c):+:x) = (((PConst a):*:b:^:c),c):(polyToList x)
polyToList ((b:^:c):+:x) = ((b:^:c),c):(polyToList x)
polyToList (a:+:b) = (polyToList a)++(polyToList b)
polyToList x = [(x, fromInteger (-1))]
