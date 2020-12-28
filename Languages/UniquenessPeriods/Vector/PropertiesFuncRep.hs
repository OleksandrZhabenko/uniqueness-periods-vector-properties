-- |
-- Module      :  Languages.UniquenessPeriods.Vector.PropertiesFuncRep
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Generalization of the functionality of the DobutokO.Poetry.Norms
-- and DobutokO.Poetry.Norms.Extended modules
-- from the @dobutokO-poetry@ package.

{-# LANGUAGE CPP, BangPatterns #-}

module Languages.UniquenessPeriods.Vector.PropertiesFuncRep (
  -- * Functions with 'Int'
  procDiverse2I
  , procDiverse2Ineg
  -- * Functions with 'Float'
  , procDiverse2F
  , procDiverse2Fneg
  , procRhythmicity23F
  , procRhythmicity23Fneg
  , procBothF
  , procBothFneg
  , procBothInvF
  , procBothInvFneg
) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__>=710
/* code that applies only to GHC 7.10.* and higher versions */
import GHC.Base (mconcat)
#endif
#endif
import qualified Data.Vector as V
import String.Languages.UniquenessPeriods.Vector
import Languages.UniquenessPeriods.Vector.PropertiesSyllables
import Languages.UniquenessPeriods.Vector.Properties
import Languages.Rhythmicity
import Languages.UniquenessPeriods.Vector.Data
import Languages.Phonetic.Ukrainian.PrepareText
import GHC.Float (int2Float)
import Melodics.Ukrainian (convertToProperUkrainian)
import MMSyn7.Syllable

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__==708
/* code that applies only to GHC 7.8.* */
mconcat = concat
#endif
#endif

procDiverse2I :: FuncRep String (UniquenessGeneral2 Char) [Int]
procDiverse2I = D2 (uniquenessPeriodsVector3 " 01-" . aux0 . convertToProperUkrainian) (justOneValue2Property . diverse2)
{-# INLINE procDiverse2I #-}

-- | Can be used to find out the minimum element.
procDiverse2Ineg :: FuncRep String (UniquenessGeneral2 Char) [Int]
procDiverse2Ineg = D2 (uniquenessPeriodsVector3 " 01-" . aux0 . convertToProperUkrainian) (justOneValue2Property . negate . diverse2)
{-# INLINE procDiverse2Ineg #-}

procDiverse2F :: FuncRep String (UniquenessGeneral2 Char) [Float]
procDiverse2F = D2 (uniquenessPeriodsVector3 " 01-" . aux0 . convertToProperUkrainian) (justOneValue2Property . int2Float . diverse2)
{-# INLINE procDiverse2F #-}

procDiverse2Fneg :: FuncRep String (UniquenessGeneral2 Char) [Float]
procDiverse2Fneg = D2 (uniquenessPeriodsVector3 " 01-" . aux0 . convertToProperUkrainian) (justOneValue2Property . int2Float . negate . diverse2)
{-# INLINE procDiverse2Fneg #-}

procRhythmicity23F :: FuncRep String (UniquenessGeneral2 Char) [Float]
procRhythmicity23F = U1 (justOneValue2Property . rhythmicity0)
{-# INLINE procRhythmicity23F #-}

-- | Can be used to find out the minimum element.
procRhythmicity23Fneg :: FuncRep String (UniquenessGeneral2 Char) [Float]
procRhythmicity23Fneg = U1 (justOneValue2Property . negate . rhythmicity0)
{-# INLINE procRhythmicity23Fneg #-}

procBothF :: FuncRep String (UniquenessGeneral2 Char) [Float]
procBothF = U1 (\xs -> let ys = convertToProperUkrainian xs in [(int2Float . diverse2 . uniquenessPeriodsVector3 " 01-" . aux0  $ ys)*(evalRhythmicity23 . mconcat . syllableDurations . map ( createSyllablesP . additionalF) . vecWords $ ys)])
{-# INLINE procBothF #-}

-- | Can be used to find out the minimum element.
procBothFneg :: FuncRep String (UniquenessGeneral2 Char) [Float]
procBothFneg = U1 (\xs -> let ys = convertToProperUkrainian xs in [(int2Float . negate . diverse2 . uniquenessPeriodsVector3 " 01-" . aux0  $ ys)*(evalRhythmicity23 . mconcat . syllableDurations . map ( createSyllablesP . additionalF) . vecWords $ ys)])
{-# INLINE procBothFneg #-}

procBothInvF :: FuncRep String (UniquenessGeneral2 Char) [Float]
procBothInvF = U1 (\xs ->
  let !ys = convertToProperUkrainian xs
      !zs = uniquenessPeriodsVector3 " 01-" . aux0  $ ys in if V.null zs then [(evalRhythmicity23 . mconcat . syllableDurations . map ( createSyllablesP . additionalF) . vecWords $ ys) * (evalRhythmicity23 . mconcat . syllableDurations . map ( createSyllablesP . additionalF) . vecWords $ ys)] else [(evalRhythmicity23 . mconcat . syllableDurations . map ( createSyllablesP . additionalF) . vecWords $ ys) / (int2Float . diverse2 $ zs)])
{-# INLINE procBothInvF #-}

-- | Can be used to find out the minimum element.
procBothInvFneg :: FuncRep String (UniquenessGeneral2 Char) [Float]
procBothInvFneg = U1 (\xs ->
  let !ys = convertToProperUkrainian xs
      !zs = uniquenessPeriodsVector3 " 01-" . aux0  $ ys in if V.null zs then [negate (evalRhythmicity23 . mconcat . syllableDurations . map ( createSyllablesP . additionalF) . vecWords $ ys) * (evalRhythmicity23 . mconcat . syllableDurations . map ( createSyllablesP . additionalF) . vecWords $ ys)] else [(evalRhythmicity23 . mconcat . syllableDurations . map ( createSyllablesP . additionalF) . vecWords $ ys) / (int2Float . negate . diverse2 $ zs)])
{-# INLINE procBothInvFneg #-}
