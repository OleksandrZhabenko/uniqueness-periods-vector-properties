-- |
-- Module      :  Languages.UniquenessPeriods.Vector.Properties
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Generalization of the functionality of the DobutokO.Poetry.Norms 
-- and DobutokO.Poetry.Norms.Extended modules 
-- from the @dobutokO-poetry@ package. 

{-# LANGUAGE BangPatterns #-}

module Languages.UniquenessPeriods.Vector.Properties where

import qualified Data.Vector as V
import String.Languages.UniquenessPeriods.Vector

-- | The function is inteded to be used after 'uniquenessPeriodsVector2' application to obtain the first argument. So generally, it is used as follows:
--  
-- > diverse . uniquenessPeriodsVector2 y whspss $ v 
-- 
-- The maximum value of the function corresponds to possibly more smoothly changing and mixing elements in the list. If they are used to represent 
-- sounds (especially some text, may be poetic) then the resulting maximum possible 'diverse' value corresponds to more \"diverse\" (phonetically) text intervals. 
-- Is somewhat similar to the @norm4@ function from the DobutokO.Poetry.Norms module from @dobutokO-poetry@ package 
-- See: https://hackage.haskell.org/package/dobutokO-poetry-general-0.1.0.0/docs/DobutokO-Poetry-Norms.html.
diverse :: 
  Eq a => UniquenessGeneral2 a -- ^ Is gotten after the application of the 'uniquenessPeriodsVector2'.
  -> Int  -- ^ The resulting value.
diverse v 
  | V.null v = 0
  | otherwise = V.sum . V.map (\(xs,_) -> if null xs then 0::Int else minimum xs) $ v

-- | The function is inteded to be used after 'uniquenessPeriodsVector2' application to obtain the first argument. So generally, it is used as follows:
--  
-- > diverse1 . uniquenessPeriodsVector2 y whspss $ v 
-- 
-- The maximum value of the function corresponds to possibly more smoothly changing and mixing elements in the list. If they are used to represent 
-- sounds (especially some text, may be poetic) then the resulting maximum possible 'diverse' value corresponds to more \"diverse\" (phonetically) text intervals. 
-- Is somewhat similar to the @norm4@ function from the DobutokO.Poetry.Norms module from @dobutokO-poetry@ package. Unlike the 'diverse' function 'diverse1' 
-- takes into account that in the word possibly there can be doubled or prolonged sounds that can be represented by repetition of the same sound 
-- representation (in some cases). These repetitions do not depend on the words order and, therefore, do not change with it so are not informative on the possible 
-- words order rearrangement that is essential to the phonetic languages as the one important application. 
-- See: https://hackage.haskell.org/package/dobutokO-poetry-general-0.1.0.0/docs/DobutokO-Poetry-Norms.html.
diverse1 :: 
  Eq a => UniquenessGeneral2 a -- ^ Is gotten after the application of the 'uniquenessPeriodsVector2'.
  -> Int  -- ^ The resulting value.
diverse1 v 
  | V.null v = 0
  | otherwise = V.sum . V.map (\(xs,_) -> if null (filter (/= 1) xs) then 0::Int else minimum (filter (/= 1) xs)) $ v

-- | Is used similarly to 'diverse1', but uses another approach based on the 'sumPositiveWithoutMax' application, so generally can give another result. 
-- It computes sum of the lists of 'Int' where each one is without the maximum value respectively and is without the elements that equal to 1. 
diverse1s :: 
  Eq a => UniquenessGeneral2 a -- ^ Is gotten after the application of the 'uniquenessPeriodsVector2'.
  -> Int  -- ^ The resulting value.
diverse1s v 
  | V.null v = 0
  | otherwise = V.sum . V.map (\(xs,_) -> if null (filter (/= 1) xs) then 0::Int else sumPositiveWithoutMax (filter (/= 1) xs)) $ v

-- | For the list of positive 'Num' elements (this is not checked so it is up to the user to check positiveness) finds out the sum of the list 
-- and the maximum value if the first argument is in the form (0, 0). For the empty list returns (0, 0) in such a case. For another first 
-- argument has more complex behaviour. Tries to be tail-recursive. 
sumPositiveAndMaxTuple :: (Num a, Ord a) => (a, a) -> [a] -> (a, a)
sumPositiveAndMaxTuple (x1,x2) (x:xs)  
  | compare x2 x == GT = sumPositiveAndMaxTuple (x1 + x, x2) xs
  | otherwise = sumPositiveAndMaxTuple (x1 + x, x) xs
sumPositiveAndMaxTuple (x1, x2) [] = (x1, x2)

-- | Unlike 'sumPositiveAndMaxTuple', it is strict by its first argument's inner elements (by both of them). 
sumPositiveAndMaxTuple' :: (Num a, Ord a) => (a, a) -> [a] -> (a, a)
sumPositiveAndMaxTuple' (!x1,!x2) (x:xs)  
  | compare x2 x == GT = sumPositiveAndMaxTuple' (x1 + x, x2) xs
  | otherwise = sumPositiveAndMaxTuple' (x1 + x, x) xs
sumPositiveAndMaxTuple' (!x1, !x2) [] = (x1, x2)

-- | For the list of positive 'Num' elements (this is not checked so it is up to the user to check positiveness) finds out the sum of the list 
-- without the maximum value. 
sumPositiveWithoutMax :: (Num a, Ord a) => [a] -> a
sumPositiveWithoutMax = uncurry (-) . sumPositiveAndMaxTuple (0, 0)

-- | The strict variant the of the 'sumPositiveWithoutMax' function.
sumPositiveWithoutMax' :: (Num a, Ord a) => [a] -> a
sumPositiveWithoutMax' = uncurry (-) . sumPositiveAndMaxTuple' (0, 0)

-- | The function is inteded to be used after 'uniquenessPeriodsVector3' application to obtain the first argument. So generally, it is used as follows:
--  
-- > diverse2 . uniquenessPeriodsVector3 whspss $ v 
-- 
-- The maximum value of the function corresponds to possibly more smoothly changing and mixing elements in the list. If they are used to represent 
-- sounds (especially some text, may be poetic) then the resulting maximum possible 'diverse2' value corresponds to more \"diverse\" (phonetically) text intervals. 
-- Is somewhat similar to the @norm4@ function from the DobutokO.Poetry.Norms module from @dobutokO-poetry@ package. Unlike the 'diverse' and 'diverse1' and 
-- 'diverse1s' functions 'diverse2' depends much more significantly on the words order. Possibly, the most accurate among \"diverse\" functions in the module.
-- See: https://hackage.haskell.org/package/dobutokO-poetry-general-0.1.0.0/docs/DobutokO-Poetry-Norms.html.
diverse2 :: 
  Eq a => UniquenessGeneral2 a -- ^ Is gotten after the application of the 'uniquenessPeriodsVector3'.
  -> Int  -- ^ The resulting value.
diverse2 v 
  | V.null v = 0
  | otherwise = V.sum . V.map (sum . fst) $ v
{-# INLINE diverse2 #-} 

-- | Is intended to be used in the 'V.Vector' @([b] -> b)@ where just the first value in the list is used. The simplest case among all possible ones. For an empty 
-- list returns 'error' with an informative message. 
oneProperty :: Ord b => [b] -> b
oneProperty xs 
 | null xs = error "Languages.UniquenessPeriods.Vector.Properties.oneProperty: empty list. "
 | otherwise = head xs
{-# INLINE oneProperty #-} 

-- | Converts just one value to the needed list to be used as a \"property\". Is provided here for explanation purposes (just as a meaningful alias). Therefore, it is
-- an auxiliary function. 
justOneValue2Property :: Ord b => b -> [b]
justOneValue2Property = (:[])
{-# INLINE justOneValue2Property #-} 
