{-# LANGUAGE DeriveDataTypeable,
             ScopedTypeVariables,
             CPP #-}

-- | A refinemente represents the partial order of partial values by a
--   tree structure. In fact the partial order is a directed acyclic
--   graph but we use a tree for simplicity.
module Test.Sloth.Refine
  (
    -- * Data Types
    Ref(..), ResRef, ArgRef, ResPos(..), ArgPos(..),

    -- * Generation of Test Cases
    refineTree,

    -- * Mapping
    fmapArgRefs, fmapResRefs,

    -- * Functions
    cut, bfs, dfs, coSeq,

    -- * Testing Polymorphic Functions
    A(..)
  ) where


import Control.Applicative
import Data.Data
import Control.Monad.State hiding ( lift )
import Control.Monad.Identity

import Test.Sloth.Pos ( Pos, (|>), bottom, root  )
import Test.Sloth.Poly ( A(..), isDataTypeA, polyA )
import Test.Sloth.Function ( isDataTypeFun )
import Test.Sloth.Generics ( Compose(..), inCompose, gunfoldWithIndex )
import Test.Sloth.CoMonad


-- | A tree that represents the demand behaviour of a function refered
-- to as refinement.
data Ref m a -- | A value and a list of refinements in the result
             = Ref a [ResRef m a]
             -- | Constructor used for non-lifted types like integers
             | NoRef [Ref m a]

-- | Annotate a value with a result position
data ResPos a = ResPos { resPos :: Pos, resRefs :: a }

-- | Annotate a value with an argument position
data ArgPos a = ArgPos { argPos :: Pos, argRefs :: a }

-- | A result refinement consists of a bottom position in the result
-- and a list of argument refinements.
type ResRef m a = ResPos [m (ArgRef m a)]

-- | An argument refinement consists of a bottom position in the
-- argument and a list of refinement trees.
type ArgRef m a = ArgPos [Ref m a]


fmapResRefs :: Functor f => (Ref f a -> Ref f b) -> [ResRef f a] -> [ResRef f b]
fmapResRefs f = map (fmap (fmapArgRefs f))

fmapArgRefs :: Functor f => (Ref f a -> Ref g b) 
            -> [f (ArgRef f a)] -> [f (ArgRef g b)] 
fmapArgRefs f = map (fmap (fmap (map f)))


instance Functor ResPos where
  fmap f (ResPos p x) = ResPos p (f x)

instance Functor ArgPos where
  fmap f (ArgPos p x) = ArgPos p (f x)

instance Functor f => Functor (Ref f) where
  fmap f (Ref v rs) = Ref (f v) (fmapResRefs (fmap f) rs)
  fmap f (NoRef rs) = NoRef (map (fmap f) rs)

instance Functor f => Applicative (Ref f) where
  pure v = Ref v []
  r1@(Ref f fs) <*> r2@(Ref v rs) =
    Ref (f v) (fmapResRefs (<*> r2) fs ++ fmapResRefs (r1 <*>) rs)
  r1@(NoRef fs) <*> r2@(NoRef rs) =
    NoRef (map (<*> r2) fs ++ map (r1 <*>) rs)
  r1 <*> NoRef rs = NoRef (map (r1 <*>) rs)
  NoRef fs <*> r2 = NoRef (map (<*> r2) fs)


-- | Generate a refinement tree for a type that is an instance of
-- Data.
refineTree :: Data a => Ref Identity a
refineTree = refineData root

-- | Generate a refinement tree for a data type where all positions
-- are relative to a provided root position.
#if MIN_VERSION_base(4,2,0)
refineData :: forall a. Data a => Pos -> Ref Identity a
refineData =
  case typeRep of
       AlgRep cs -> refineAlgRep dt cs
       IntRep    -> fmap (fromConstr . mkIntegralConstr dt) . intRef
       FloatRep  -> error "refineData: floats not implemented"
       CharRep   -> fmap (fromConstr . mkCharConstr dt) . charRef
       NoRep     -> error "refineData: used a data type with no representation"
  where
   dt = dataTypeOf (undefined :: a)
   typeRep = dataTypeRep dt
#else
refineData :: forall a. Data a => Pos -> Ref a
refineData =
  case typeRep of
       AlgRep cs -> refineAlgRep dt cs
       IntRep    -> fmap (fromConstr . mkIntConstr dt) . intRef
       FloatRep  -> error "refineData: floats not implemented"
       StringRep -> lifted [cons0 empty, cons2 cons]
       NoRep     -> error "refineData: data type with no representation"
  where
   dt = dataTypeOf (undefined :: a)
   aDt = dataTypeOf (undefined :: A)
   typeRep = dataTypeRep dt
   empty = fromConstr (mkStringConstr dt [])
   cons x xs = fromConstr (mkStringConstr dt (x:xs))
#endif

-- | Generate refinement tree in the case that the representation of
-- the type is an AlgRep.
refineAlgRep :: Data a => DataType -> [Constr] -> Pos -> Ref Identity a
refineAlgRep dt cs
  | isDataTypeA dt   = pure . polyA
  | isDataTypeFun dt = unlifted (map refineConstr cs)
  | otherwise        = lifted (map refineConstr cs)
 where
  refineConstr =
    compose . gunfoldWithIndex (inCompose . refineArgument) (Compose . cons0)

  refineArgument n f p = f p <*> refineData (p|>n)

-- | Construct refinement tree from list of children by adding bottom
-- as root element
lifted :: [Pos -> Ref Identity a] -> Pos -> Ref Identity a
lifted rs p = Ref (bottom p) [ResPos p [return (ArgPos p (map ($p) rs))]]

-- | Construct refinement tree from list of children without adding
-- bottom as root element
unlifted :: [Pos -> Ref m a] -> Pos -> Ref m a
unlifted rs p = NoRef (map ($p) rs)

-- | Contruct a refinement for a value
cons0 ::  a -> Pos -> Ref Identity a
cons0 x _ = pure x

-- | Construct a refinement for a unary constructor
cons1 :: Data a => (a -> b) -> Pos -> Ref Identity b
cons1 c p = c <$> refineData (p|>0)

-- | Construct a refinement for a twoary constructor
cons2 :: (Data a,Data b) => (a -> b -> c) -> Pos -> Ref Identity c
cons2 c p = cons1 c p <*> refineData (p|>1)

-- | A refinement for integers
intRef :: Pos -> Ref Identity Int
intRef = lifted [cons0 0,linearRef pred (-1),linearRef succ 1]

-- | A refinement where each node has one value and one successor
linearRef :: (a -> a) -> a -> Pos -> Ref Identity a
linearRef f = linearRef'
 where
  linearRef' x = unlifted [cons0 x,linearRef' (f x)]

-- | A refinement tree for characters
charRef :: Pos -> Ref Identity Char
charRef =
  lifted [cons0 '?',fmap (\i -> toEnum (63-i)) . natRef,
                    fmap (\i -> toEnum (63+i)) . natRef]
 where
  natRef = boundedNatRef 0 63

-- | A refinement for integers between two bounds
boundedNatRef :: Int -> Int -> Pos -> Ref Identity Int
boundedNatRef minNat maxNat = unlifted (boundedNatRef' 1)
 where
  boundedNatRef' x
    | minNat<=x && x<=maxNat = [cons0 x,unlifted (boundedNatRef' (2*x)),
                                        unlifted (boundedNatRef' (succ (2*x)))]
    | otherwise              = []

-- | Cut a refinement tree at a given depth
cut :: forall a f. Functor f => Int -> Ref f a -> Ref f a
cut d (NoRef rs) = NoRef (if d==0 then [] else map (cut (d-1)) rs)
cut d (Ref x rs) = Ref x (map (fmap cut') rs)
 where
  cut' :: [f (ArgRef f a)] -> [f (ArgRef f a)]
  cut' rs'
    | d <= 0    = []
    | otherwise = fmapArgRefs (cut (d-1)) rs'

-- | Breadth first search on a refinement tree
bfs :: forall a f. CoPointed f => Ref f a -> [a]
bfs t = unfoldOrs [t]
 where
  partition :: Ref f a -> ([a],[Ref f a]) -> ([a],[Ref f a]) 
  partition (Ref v rs) ~(vs,ors) =
    (v:vs,concatMap (argRefs . extract) (concatMap resRefs rs)++ors)
  partition (NoRef rs) ~(vs,ors) = (vs,rs++ors)

  unfoldOrs [] = []
  unfoldOrs (x:xs) = let (vals,ors) = foldr partition ([],[]) (x:xs) in
    vals ++ unfoldOrs ors

-- | Depth first search on a refinement tree
dfs :: CoPointed f => Ref f a -> [a]
dfs (NoRef rs) = concatMap dfs rs
dfs (Ref v rs) =
  v:concatMap dfs (concatMap (argRefs . extract) (concatMap resRefs rs))

-- | A refinement tree with effects around the successsors is
-- transformed into a tree with effects around the entries
coSeq :: (Monad m,CoMonad m) => Ref m a -> Ref Identity (m a)
coSeq = coSeqRef return

coSeqRef :: (Monad m,CoMonad m) => (a -> m a) -> Ref m a
          -> Ref Identity (m a)
coSeqRef f (NoRef rs) = NoRef (map (coSeqRef f) rs)
coSeqRef f (Ref v rs) = Ref (f v) (map (fmap (map (Identity . coSeqArgRef f))) rs)

coSeqArgRef :: (Monad m,CoMonad m) => (a -> m a) -> m (ArgRef m a) 
            -> ArgRef Identity (m a)
coSeqArgRef f ft = fmap (map (coSeqRef (f >=> (ft <<=) . const))) (extract ft)
