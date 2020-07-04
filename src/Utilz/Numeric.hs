{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}


module Utilz.Numeric
(   Approx (..)
,   dot
,   (+^), (-^), (*^), (/^), interp
,   grad
,   negGrad
,   DVec
) where

import Data.List (foldl')
import Data.Vector.Unboxed ((!),(//))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.ST
import Data.Time (Day)

import Debug.Trace (trace)
debug = flip trace

type DVec = U.Vector Double

class Approx a where 
    (=~) :: a -> a -> Bool 
    (=~) x y = not $ (/~) x y

    (/~) :: a -> a -> Bool
    (/~) x y = not $ (=~) x y

instance Approx Day where (!x) =~ (!y) = x == y

instance Approx Double where
    x =~ y = if (mx < 1e-5) || (abs (x-y)) / mx < 1e-7 then True else False 
        where mx = (max (abs x) (abs y))

instance Approx a => Approx (Maybe a) where
    Nothing =~ Nothing  =   True
    Nothing =~ Just _   =   False
    Just _ =~ Nothing   =   False 
    Just x =~ Just y    =   x =~ y 

instance Approx a => Approx [a] where 
    x =~ y = foldr (&&) True $ zipWith (=~) x y

instance (Approx a, Approx b) => Approx (a, b) where
    (x,y) =~ (a,b) = (x =~ a) && (y =~ b)

instance (Approx a, Approx b, Approx c) => Approx (a, b, c) where
    (x,y,z) =~ (a,b,c) = (x =~ a) && (y =~ b) && (z =~ c)

instance (M.Unbox a, Approx a) => Approx (U.Vector a) where 
    x =~ y = U.foldr (&&) True $ U.zipWith (=~) x y

dot x y = U.sum $ U.zipWith (*) x y

(+^) x y = U.zipWith (+) x y
(-^) x y = U.zipWith (-) x y
(*^) x s = U.map (s*) x
(/^) x s = U.map (/s) x

interp :: DVec -> DVec -> Double -> DVec
interp x y s = U.zipWith (\xi yi -> (1.0-s)*xi + s*yi) x y

grad :: (DVec -> Double) -> DVec -> DVec
grad f !v = runST $ do
    let !f0 = f v
    U.forM (U.fromList [0..((U.length v) - 1)]) $ \i -> do
        let !vi = v ! i; !dvm = vi*1e-8; !z = v // [(i, vi + dvm)] 
        return (((f z) - f0)/dvm) -- `debug` ("hi " ++ show z)

negGrad :: (DVec -> Double) -> DVec -> DVec
negGrad f !v = runST $ do
    let !f0 = f v
    U.forM (U.fromList [0..((U.length v) - 1)]) $ \i -> do
        let !vi = v ! i; !dvm = vi*1e-8; !z = v // [(i, vi - dvm)] 
        return (((f z) - f0)/dvm) -- `debug` ("hi " ++ show z)
