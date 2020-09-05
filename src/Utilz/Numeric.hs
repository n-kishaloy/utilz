{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Strict #-}


module Utilz.Numeric
( Approx (..)
, dot
, (+^), (-^), (*^), (/^), interp
, grad
, negGrad
, DVec
) where

import Data.List (foldl')
import Data.Vector.Unboxed ((!),(//))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector as V
import Control.Monad.ST
import Data.Time (Day)
import qualified Data.HashMap.Strict as Hm
import Data.Hashable
import Data.List (foldl')
import Data.Text (Text)

import Debug.Trace (trace)
debug = flip trace

type DVec = U.Vector Double

class Approx a where 
  (=~) :: a -> a -> Bool 
  (=~) x y = not $ (/~) x y

  (/~) :: a -> a -> Bool
  (/~) x y = not $ (=~) x y

instance Approx Day where x =~ y = x == y

instance Approx Char where x =~ y = x == y

instance Approx Bool where x =~ y = x == y

instance Approx Text where x =~ y = x == y

instance Approx Int where x =~ y = x == y

instance Approx Integer where x =~ y = x == y

instance Approx Float where
  x =~ y = if (mx < 1e-5) || (abs (x-y)) / mx < 1e-7 then True else False 
    where mx = (max (abs x) (abs y))

instance Approx Double where
  x =~ y = if (mx < 1e-5) || (abs (x-y)) / mx < 1e-7 then True else False 
    where mx = (max (abs x) (abs y))

instance Approx a => Approx (Maybe a) where
  Nothing =~ Nothing  =   True
  Nothing =~ Just _   =   False
  Just _ =~ Nothing   =   False 
  Just x =~ Just y    =   x =~ y 

instance Approx a => Approx [a] where 
  x =~ y = (length x == length y) && (foldr (&&) True $ zipWith (=~) x y)

instance (Approx a, Approx b) => Approx (a, b) where
  (x,y) =~ (a,b) = (x =~ a) && (y =~ b)

instance (Approx a, Approx b, Approx c) => Approx (a, b, c) where
  (x,y,z) =~ (a,b,c) = (x =~ a) && (y =~ b) && (z =~ c)

instance (Approx a,Approx b,Approx c,Approx d) => Approx (a,b,c,d) where
  (x,y,z,u) =~ (a,b,c,d) = (x =~ a) && (y =~ b) && (z =~ c) && (u =~ d)

instance (M.Unbox a, Approx a) => Approx (U.Vector a) where 
  x =~ y = (U.length x==U.length y) && (U.foldr (&&) True $ U.zipWith (=~) x y)

instance (Approx a) => Approx (V.Vector a) where 
  x =~ y = (V.length x==V.length y) && (V.foldr (&&) True $ V.zipWith (=~) x y)

instance (Eq a, Hashable a, Approx b) => Approx (Hm.HashMap a b) where
  x =~ y = (fz x y) && (fz y x) where
    fz p q = foldl' (f p) True $ Hm.toList q
    f p t z = t && ((Hm.lookup k p) =~ (Just v)) where (k,v) = z 


dot x y = U.sum $ U.zipWith (*) x y

(+^) x y = U.zipWith (+) x y
(-^) x y = U.zipWith (-) x y
(*^) x s = U.map (s*) x
(/^) x s = U.map (/s) x

interp :: DVec -> DVec -> Double -> DVec
interp x y s = U.zipWith (\xi yi -> (1.0-s)*xi + s*yi) x y

grad :: (DVec -> Double) -> DVec -> DVec
grad f v = runST $ do
  let f0 = f v
  U.forM (U.fromList [0..((U.length v) - 1)]) $ \i -> do
    let vi = v ! i; dvm = vi*1e-8; z = v // [(i, vi + dvm)] 
    return (((f z) - f0)/dvm) -- `debug` ("hi " ++ show z)

negGrad :: (DVec -> Double) -> DVec -> DVec
negGrad f v = runST $ do
  let f0 = f v
  U.forM (U.fromList [0..((U.length v) - 1)]) $ \i -> do
    let vi = v ! i; dvm = vi*1e-8; z = v // [(i, vi - dvm)] 
    return (((f z) - f0)/dvm) -- `debug` ("hi " ++ show z)
