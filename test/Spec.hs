{-# LANGUAGE NumericUnderscores #-}

module Main where

import Test.QuickCheck 

import Utilz ((~=))

main :: IO ()
main = do 

    print "Float equality test"
    quickCheck $ 0.0 ~= (-1e-8)
    quickCheck $ 0.0 ~= (-0.0)
    quickCheck $ 0.0 ~= 0.000
    quickCheck $ 1e+7 ~= 10_000_000.05


