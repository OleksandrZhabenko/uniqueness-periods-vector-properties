-- |
-- Module      :  Languages.UniquenessPeriods.Vector.PropertiesSyllables
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Generalization and extension of the functionality of the DobutokO.Poetry.Norms
-- and DobutokO.Poetry.Norms.Extended modules
-- from the @dobutokO-poetry@ package. Uses syllables information.

{-# LANGUAGE CPP #-}

module Languages.UniquenessPeriods.Vector.PropertiesSyllables (
  -- * Rhythmicity metrices
  -- ** A simple one
  rhythmicity0
  -- ** With weight coefficients
  , rhythmicityK
) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import qualified Data.Vector as V
import Languages.Rhythmicity
import MMSyn7.Syllable

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

rhythmicity0 :: String -> Float
rhythmicity0 xs
 | null xs = 0.0
 | otherwise = evalRhythmicity23 . mconcat . syllableDurations . syllablesUkrP $ xs

rhythmicityK :: Float -> Float -> String -> Float
rhythmicityK k2 k3 xs
 | null xs = 0.0
 | otherwise = evalRhythmicity23K k2 k3 . mconcat . syllableDurations . syllablesUkrP $ xs
