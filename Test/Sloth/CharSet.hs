{-# LANGUAGE FlexibleInstances,
             IncoherentInstances #-}

-- | An approximation of the characteristic set
module Test.Sloth.CharSet
  (
    Testable(..), checkCharSet, pruneSet
  ) where


import Control.Monad.Identity
import Data.Data ( Data )
import Data.List ( find, intersect, partition )
import Control.Monad.Writer
import Control.Monad.List

import Test.Sloth.CoMonad
import Test.Sloth.PVal
import Test.Sloth.Pos ( Pos, root )
import Test.Sloth.TestCase
import Test.Sloth.Refine
import Test.Sloth.Search ( Search(..), isComplete, completion, filterIncomplete,
                           SearchT(..), zero, one )
import Test.Sloth.Config ( Config(..) )
import Test.Sloth.Function ( fun2, fun3, fun4, fun5, fun6, fun7 )


class Testable fun where
  -- | Approximate the characteristic set for a function up to a given
  -- depth
  charSet :: fun -> Bool -> Int -> Ref Search Mapping

instance (Data a,Data b) => Testable (a -> b) where
  charSet = charSet'

instance (Data a,Data b,Data c) => Testable (a -> b -> c) where
  charSet f = charSet' (fun2 f)

instance (Data a,Data b,Data c,Data d) => Testable (a -> b -> c -> d) where
  charSet f = charSet' (fun3 f)

instance (Data a,Data b,Data c,Data d,Data e) =>
          Testable (a -> b -> c -> d -> e) where
  charSet f = charSet' (fun4 f)

instance (Data a,Data b,Data c,Data d,Data e,Data f) =>
         Testable (a -> b -> c -> d -> e -> f) where
  charSet f = charSet' (fun5 f)

instance (Data a,Data b,Data c,Data d,Data e,Data f,Data g) =>
         Testable (a -> b -> c -> d -> e -> f -> g) where
  charSet f = charSet' (fun6 f)

instance (Data a,Data b,Data c,Data d,Data e,Data f,Data g,Data h) =>
         Testable (a -> b -> c -> d -> e -> f -> g -> h) where
  charSet f = charSet' (fun7 f)


-- | Approximate the characteristic set of a given function
charSet' :: (Data a,Data b) => (a -> b) -> Bool -> Int -> Ref Search Mapping
charSet' f simp n
  | simp      = filterDemanded ref
  | otherwise = filterSeq ref
 where
  ref = identifySeqPos (wholeSet f (cut n refineTree))

-- | Compute a superset of the characteristic set. This set still
-- contains non-sequential refinements.
wholeSet :: (Data a,Data b) => (a -> b) -> Ref Identity a -> Ref Identity Mapping
wholeSet = wholeSet' root

wholeSet' :: (Data a,Data b) => Pos -> (a -> b) -> Ref Identity a 
          -> Ref Identity Mapping
wholeSet' rp f (NoRef rs) = NoRef (map (wholeSet' rp f) rs)
wholeSet' rp f (Ref x rs) =
  Ref m [ ResPos rp' (fmapArgRefs (wholeSet' rp' f) ars) | rp' <- rps ]
 where
  m = mapping f x
  fpv = result m
  rps = botPosGE fpv rp
  ars = concatMap resRefs rs

-- | Identify argument refinements that are definitely sequential. The
-- first argument refinement is a refinement at the demanded position.
identifySeqPos :: Ref Identity Mapping -> Ref Search Mapping
identifySeqPos (Ref m rs) = Ref m (map (identifySeqPosResRef (result m)) rs)
identifySeqPos (NoRef rs) = NoRef (map identifySeqPos rs)

identifySeqPosResRef :: PVal -> ResRef Identity Mapping -> ResRef Search Mapping
identifySeqPosResRef _   (ResPos rp []) = ResPos rp []
identifySeqPosResRef fpv (ResPos rp ars) =
  ResPos rp (Complete ar:identifyDerivedSeqPos rp ar (filter ((/=p) . argPos) ars'))
 where
  Bottom p = sel fpv rp
  ars' = map extract (fmapArgRefs identifySeqPos ars)
  Just ar = find ((==p) . argPos) ars'

-- | A position is definitely sequential if it is sequential in all
-- successors
identifyDerivedSeqPos :: Pos -> ArgRef Search a -> [ArgRef Search a] 
                      -> [Search (ArgRef Search a)]
identifyDerivedSeqPos rp ar ars = map Complete cs ++ map Incomplete is
 where
  (cs,is) = partition ((`elem` seqPs) . argPos) ars
  seqPs = commonDefiniteSeqPos rp ar

-- | Definite Positions that have all children in common
commonDefiniteSeqPos :: Pos -> ArgRef Search a -> [Pos]
commonDefiniteSeqPos rp (ArgPos _ rs) = intersectAll (map (defSeq . refs) rs)
 where
  defSeq = concatMap (definiteSeqPos . resRefs) . filter ((==rp) . resPos)
  refs (Ref _ rrs) = rrs
  refs _           = []

-- | Argument positions definitely identified as beeing sequential
definiteSeqPos :: [Search (ArgRef m a)] -> [Pos]
definiteSeqPos = map (argPos . extract) . filter isComplete

-- | Remove all definitely and potially non-sequential steps
filterDemanded :: Ref Search Mapping -> Ref Search Mapping
filterDemanded (Ref m rs) = Ref m (map defSeqArgRef rs)
 where
  defSeqArgRef (ResPos rp ars) = 
    ResPos rp (filter isComplete (fmapArgRefs filterDemanded ars))
filterDemanded (NoRef rs) = NoRef (map filterDemanded rs)

-- | Remove all definitely non-sequential steps from a set of
-- refinement steps
filterSeq :: Ref Search Mapping -> Ref Search Mapping
filterSeq (Ref m rs) = Ref m (map filterSeqResRef rs)
filterSeq (NoRef rs) = NoRef (map filterSeq rs)

filterSeqResRef :: ResRef Search Mapping -> ResRef Search Mapping
filterSeqResRef r = 
  ResPos rp (filterIncomplete isSeqPos (fmapArgRefs filterSeq ars))
 where
  ResPos rp ars = r
  ars' = map extract ars
  isSeqPos ar = completion not (liftM (notElem (argPos ar)) nonSeqPos)
  nonSeqPos = liftM (concatMap (botPos . argument))
                    (extract (runSearchT (runListT (valuesArgRef rp ars'))))

-- | Check a characteristic set for counter-examples
checkCharSet :: Ref Search Mapping -> Ref Search TestCase
checkCharSet r@(Ref m rs) = 
  Ref (checkMapping m (map resPos rs) r) (fmapResRefs checkCharSet rs)
checkCharSet (NoRef rs) = NoRef (map checkCharSet rs)

checkMapping :: Mapping -> [Pos] -> Ref Search Mapping -> TestCase
checkMapping (Mapping pv fpv) rps r =
  TestCase (Mapping pv fpv) (map (inf pv r) rps)

-- | Compute the infimum for a result position
inf :: PVal -> Ref Search Mapping -> Pos 
    -> (Pos,SearchT (Writer (Sum Int)) PVal)
inf pv r rp = 
  (rp, liftM (infAtPos rp) (runListT (values rp r)))
 where
  infAtPos rp' = mapPoly (proj pv) . infimum . map (flip sel rp' . result)

-- | Collect the mappings that are defined at a given position
values :: CoPointed f => Pos -> Ref f Mapping 
       -> ListT (SearchT (Writer (Sum Int))) Mapping
values _  (NoRef []) = ListT (zero [])
values rp (NoRef rs) = ListT (return rs) >>= values rp
values rp (Ref m rs)
  | isBottom (sel fpv rp) = valuesArgRef rp (map extract (resRefs r))
  | otherwise             = ListT (one [m])
 where
  fpv = result m
  Just r = find ((==rp) . resPos) rs

valuesArgRef :: CoPointed f => Pos -> [ArgRef f Mapping]
             -> ListT (SearchT (Writer (Sum Int))) Mapping
valuesArgRef _  [] = ListT (zero [])
valuesArgRef rp rs = ListT (return (firstArgRefs rs)) >>= values rp
 where
  firstArgRefs (ArgPos _ rrs:_) = rrs
  firstArgRefs _                = error "valuesArgRef: empty argument refinement"

-- | Prune a refinement by looking for leftmost, topmost
-- counter-examples. We prune up to some depth and increase the depth
-- each time
pruneSet  :: Int -> Config -> Ref Identity (Search TestCase) 
          -> Ref Identity (Search TestCase)
pruneSet n config tree =
  fst (iterate (\(t,d) -> (pruneSetUpTo d config t,d+1)) (tree,1) !! n)

-- | Prune a refinement up to a given depth
pruneSetUpTo :: Int -> Config -> Ref Identity (Search TestCase)
             -> Ref Identity (Search TestCase)
pruneSetUpTo n config tree = fst (runWriter (pruneSetWriter n config tree))

pruneSetWriter :: Int -> Config -> Ref Identity (Search TestCase)
               -> Writer Any (Ref Identity (Search TestCase))
pruneSetWriter n config (NoRef rs) = 
  liftM NoRef (mapM (pruneSetWriter (n-1) config) rs)
pruneSetWriter n config (Ref t rs) = do
  tell (Any (isFailure config (extract t)))
  liftM (Ref t) (mapM (pruneResRef n config) rs)

pruneResRef :: Int -> Config -> ResRef Identity (Search TestCase) 
            -> Writer Any (ResRef Identity (Search TestCase))
pruneResRef _ _      r@(ResPos _ [])  = return r
pruneResRef n config r@(ResPos rp ars)
  | n==0      = return r
  | otherwise =
  case writers of
       []  -> return (ResPos rp [head ars])
       w:_ -> liftM (\ar -> ResPos rp [return ar]) w
 where
  writers = filter (getAny . execWriter) (map (pruneArgRef n config . extract) ars)

pruneArgRef :: Int -> Config -> ArgRef Identity (Search TestCase) 
            -> Writer Any (ArgRef Identity (Search TestCase))
pruneArgRef n config (ArgPos p rs) =
  liftM (ArgPos p) (mapM (pruneSetWriter (n-1) config) rs)


-- * Utility Functions

-- | Elementwise intersection of a list of lists
intersectAll :: Eq a => [[a]] -> [a]
intersectAll [] = []
intersectAll xs = foldr1 intersect xs
