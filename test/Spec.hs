{-# LANGUAGE NumericUnderscores, OverloadedStrings #-}

module Main where

import Test.QuickCheck 

import Utilz.Numeric ((=~),(/~),dot,(+^), (-^), (*^), (/^))
import qualified Utilz as Ut
import qualified Utilz.Numeric as Nu
import qualified Utilz.Numeric.Optima as Op

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Vector.Unboxed  ((!))
import Data.Time (Day, fromGregorian)
import qualified Data.HashMap.Strict as Hm
import Data.Text (Text)



main :: IO ()
main = do 

  print "Float equality test"
  quickCheck $ (0.0 :: Double) =~ (-1e-8)
  quickCheck $ (0.0 :: Double) =~ (-0.0)
  quickCheck $ (1.0e+7 :: Double) =~ 10_000_000.05

  quickCheck $ Just (10.00000000000007 :: Double) =~ Just 10.0
  quickCheck $ Just (10 :: Double) /~ Just 1.0
  quickCheck $ Just (10 :: Double) /~ Nothing
  quickCheck $ (Nothing :: Maybe Double) /~ Just 1.0

  quickCheck $ ([1.2, 3.4, 5.6] :: [Double]) /~ [1.2, 3.4, 5.65]
  quickCheck $ ([2.3, 1,2, 1000] :: [Double]) =~ [2.3, 1,2, 1e+3]
  quickCheck $ ([1.0,1.0,1.0,1.0] :: [Double]) /~ [1.0,1.0,1.0,1.0,2.0]

  quickCheck $ ((1.2, 3.4) :: (Double, Double)) =~ (1.20000008, 3.399999999)
  quickCheck $ ((1.2, 3.5) :: (Double, Double)) /~ (1.20000008, 3.399999999)

  quickCheck $ ((1.2, 3.4, 2.5) :: (Double, Double, Double)) =~ (1.20000008, 3.399999999, 2.5000001)
  quickCheck $ ((1.2, 3.4, 2.5) :: (Double, Double, Double)) /~ (1.2, 3.399999999, 2.4)

  quickCheck $ ((1.2, 3.4, 2.5) :: (Float, Float, Float)) =~ (1.20000008, 3.399999999, 2.5000001)
  quickCheck $ ((1.2, 3.4, 2.5) :: (Float, Float, Float)) /~ (1.2, 3.399999999, 2.4)

  quickCheck $ ((1, 3, 2) :: (Int, Int, Int)) =~ (1, 3, 2)
  quickCheck $ ((1, 3, 2) :: (Int, Int, Int)) /~ (1, 3, 5)



  quickCheck $ False /~ True
  quickCheck $ True =~ True
  quickCheck $ False =~ False


  quickCheck $ ("star"::Text) =~ ("star"::Text)
  quickCheck $ ("star"::Text) /~ ("start"::Text)
  quickCheck $ ("start"::Text) /~ ("star"::Text)

  quickCheck $ ("star"::String) =~ ("star"::String)
  quickCheck $ ("star"::String) /~ ("start"::String)
  quickCheck $ ("start"::String) /~ ("star"::String)

  quickCheck $ (fromGregorian 2018 3 30) =~ (fromGregorian 2018 3 30)
  quickCheck $ (fromGregorian 2018 3 31) /~ (fromGregorian 2018 3 30)

  let rb = Hm.fromList [("Cash", 10.23), ("CurrentAdvances", 56.25), ("AccountPayables", 0.0)] :: Hm.HashMap String Double
  let tb = Hm.fromList [("Cash", 10.23), ("CurrentAdvances", 56.25)] :: Hm.HashMap String Double

  quickCheck $ rb /~ tb
  quickCheck $ tb /~ rb

  let rb = Hm.fromList [("Cash", 10.23), ("CurrentAdvances", 56.25)] :: Hm.HashMap String Double

  quickCheck $ rb =~ tb
  quickCheck $ tb =~ rb

  let rb = Hm.fromList [("Cash", Just 10.23), ("CurrentAdvances", Just 56.25), ("AccountPayables", Just 0.0)] :: Hm.HashMap String (Maybe Double)
  let tb = Hm.fromList [("Cash", Just 10.23), ("CurrentAdvances", Just 56.25)] :: Hm.HashMap String (Maybe Double)

  quickCheck $ rb /~ tb
  quickCheck $ tb /~ rb

  let rb = Hm.fromList [("Cash", Just 10.23), ("CurrentAdvances", Just 56.25)] :: Hm.HashMap String (Maybe Double)

  quickCheck $ rb =~ tb
  quickCheck $ tb =~ rb

  let rb = V.fromList [("Cash", Just 10.23), ("CurrentAdvances", Just 56.25), ("AccountPayables", Just 0.0)] :: V.Vector (String, Maybe Double)
  let tb = V.fromList [("Cash", Just 10.23), ("CurrentAdvances", Just 56.25)] :: V.Vector (String, Maybe Double)

  quickCheck $ rb /~ tb

  let rb = V.fromList [("Cash", Just 10.24), ("CurrentAdvances", Just 56.25)] :: V.Vector (String, Maybe Double)

  quickCheck $ rb /~ tb

  print ""; print $ "Newton Raphson soln"
  quickCheck $ Op.newtRaph (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
  quickCheck $ Op.newtRaph (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667
  quickCheck $ Op.newtRP (\x -> 12*x^4 -5*x^3 + 5) 400 =~ Nothing
  quickCheck $ Op.newtRP (\x -> 12*x^4 -5*x^3) 400 =~ Just 0.416666666667

  print ""; print $ "Vector maths"

  let v1 = U.fromList [1.2,3.4,4.5] :: U.Vector Double
  let v2 = U.fromList [2.5,3.6,1.2] :: U.Vector Double

  putStr "Dot product : "; quickCheck $ v1 `dot` v2 =~ 20.64

  putStr "Addition : "; quickCheck $ v1 +^ v2 =~ U.fromList [3.7,7.0,5.7]
  putStr "Addition : "; quickCheck $ v1 +^ v2 /~ U.fromList [3.75,7.0,5.7]

  putStr "Subtraction : "; quickCheck $ v1 -^ v2 =~ U.fromList [-1.3,-0.2,3.3]
  putStr "Subtraction : "; quickCheck $ v1 -^ v2 /~ U.fromList [-1.3,-0.2,3.2]

  putStr "Multiply:"; quickCheck $ v1 *^ (-2.0) =~ U.fromList [-2.4,-6.8,-9.0]

  putStr "Divide : "; quickCheck $ v1 /^ 0.5 =~ U.fromList [2.4,6.8,9.0]

  putStr "Interp : "; quickCheck $ Nu.interp (U.fromList [2.0,3.0]) (U.fromList [10.0,15.0]) 0.25 =~   (U.fromList [4.0,6.0])

  putStr "Grad : "; quickCheck $ Nu.grad (\x -> (x ! 0) - (x ! 1)*5.0 + (x ! 2)**2) (U.fromList [5.0,2.0,-4.0]) =~ U.fromList [1.0,-5.0,-8.0]

  putStr "NegGrad : "; quickCheck $ Nu.negGrad (\x -> (x ! 0) - (x ! 1)*5.0 + (x ! 2)**2) (U.fromList [5.0,2.0,-4.0]) =~ U.fromList [-1.0,5.0,8.0]

  putStr "BPhase : "; quickCheck $ (Op.bPhase (
    \x -> (((x ! 0)-2.5)**2.0/25.0 + ((x ! 1)-4.5)**2.0/100.0)) 
      (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5])) =~ Just (0.4096,1.6384)

  putStr "LineSearch : "; quickCheck $ ((Op.lineSearch (
    \x -> (((x ! 0)-2.5)**2.0/25.0 + ((x ! 1)-4.5)**2.0/100.0)) 
    (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5])) 0.512 2.048) =~ (Just $ U.fromList [2.323530954719283,3.0882345226403585])

  putStr "Line Optima : "; quickCheck $ (Op.lineOptima (
    \x -> (((x ! 0)-2.5)**2.0/25.0 + ((x ! 1)-4.5)**2.0/100.0)) 
      (U.fromList [3.5, 2.5]) (U.fromList [-1.0, 0.5])) =~ (Just $ U.fromList [2.3235300091300894,3.0882349954349553])

  putStr "Conj Grad : "; quickCheck $ Op.conjGradPR (
    \x -> ((x ! 0) - 3.0)**4.0 + ((x ! 1) - 4.0)**2.0 + ((x ! 2) - 2.0)**2.0 + ((x ! 2) - 2.0)**4.0 + 10.0 ) (U.fromList [4.2,2.0,0.75]) =~ (Just $ U.fromList [2.971601975980278,3.999995704367093,1.999991592551973])


    