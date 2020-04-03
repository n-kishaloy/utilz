{-# LANGUAGE NumericUnderscores #-}


module Utilz
    (   (~=)
    
    ) where

(~=) :: Double -> Double -> Bool 
(~=) x y = 
    if (mx < 1e-5) || (abs (x-y)) / mx < 1e-7 then True else False where 
        mx = (max (abs x) (abs y))


