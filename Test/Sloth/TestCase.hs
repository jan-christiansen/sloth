module Test.Sloth.TestCase
  (
  Mapping(..), mapping,

  TestCase(..), showTestCase, isFailure, isValid
  ) where


import Control.Monad.Writer ( execWriter, Writer )
import Data.Monoid ( Sum(..) )
import Data.Data ( Data )

import Test.Sloth.CoMonad
import Test.Sloth.PVal ( PVal, toPVal, showsPrecPValWithSub, isBottom,
                         simplifyPos, mapPoly )
import Test.Sloth.Pos ( Pos )
import Test.Sloth.Search ( Search, isComplete, SearchT(..), isCompleteT )
import Test.Sloth.Config ( Config(..) )
import Test.Sloth.Color ( Color(..), showColor )


-- | A Mapping consists of a argument value and the corresponding
-- result
data Mapping = Mapping { argument :: PVal, result :: PVal }

instance Show Mapping where
  show (Mapping pv fpv) = "\\" ++ showsPrec 11 pv " -> " ++ show fpv

-- | Construct a Mapping from a function and an argument
mapping :: (Data a,Data b) => (a -> b) -> a -> Mapping
mapping f v = Mapping pv fpv
 where
  pv = toPVal v
  fpv = toPVal (f v)

-- | A TestCase is a Mapping and a list of tested result positions
-- with corresponding suggested results of these positions
data TestCase = TestCase Mapping [(Pos,SearchT (Writer (Sum Int)) PVal)]

-- | 
showTestCase :: Config -> Search TestCase -> String
showTestCase config stc = showTC (extract stc) ""
 where
  showTC (TestCase (Mapping pv fpv) l) =
     showsTestCase (detailed config)
                   (isComplete stc)
                   (showsPrec 11 pv')
                   (showsPrec 0 fpv')
                   (showsPrecPValWithSub sub 0 fpv')
    where
     (pv',f) = simplifyPos pv
     fpv' = mapPoly f fpv
     sub = map (fmap (showsPrecSearchPVal config . fmap (mapPoly f))) l

-- | 
showsTestCase :: Bool -> Bool -> ShowS -> ShowS -> ShowS -> ShowS
showsTestCase detail definite arg current proposed
  | detail =
     showString (if definite then "Argument(s): " else "Potential Argument(s): ")
       . arg
       . showString "\nCurrent Result: " . current
       . showString "\nProposed Result: " . proposed
  | otherwise =
      (if definite then id else showColor Blue) 
        (showString "\\" . arg . showString " -> " . proposed)

-- |
showsPrecSearchPVal :: Config -> SearchT (Writer (Sum Int)) PVal -> Int -> ShowS
showsPrecSearchPVal config searcht prec
  | not (colored config) = showsPrec prec pv
  | samples == 0         = showChar '?'
  | isBottom pv          = showColor Green (shows pv)
  | isCompleteT searcht  = showColor Red (showsPrec prec pv)
  | samples < minSamples = showChar '?'
  | otherwise            = showColor Magenta (showsPrec prec pv)
 where
  samples = getSum (execWriter (runSearchT searcht))
  pv = extract searcht
  minSamples = minInfSize config

-- | Check whether a TestCase is a failure given a certain
-- configuration
isFailure :: Config -> TestCase -> Bool
isFailure _      (TestCase _ []) = False
isFailure config (TestCase _ l)  =
  any (\(_,s) -> not (isBottom (extract s))
         && (isCompleteT s || getSum (execWriter (runSearchT s)) >= minSamples)) l
 where
  minSamples = minInfSize config

-- | Check whether a TestCase should be displayed under a
-- configuration
isValid :: Config -> TestCase -> Bool
isValid config (TestCase _ []) = noBottomPos config
isValid config testcase = successes config || isFailure config testcase
