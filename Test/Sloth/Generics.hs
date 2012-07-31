{-# LANGUAGE Rank2Types #-}
module Test.Sloth.Generics
  (
  Compose(..), inCompose,

  fromConstrWithChildren,

  gunfoldM, gunfoldWithIndex
  ) where


import Control.Monad.State ( evalState, get, put )
import Data.Data ( Data, Constr, gunfold, fromConstrM )


-- | Compose two type constructors
newtype Compose f g a = Compose { compose :: f (g a) }

-- | Lift a function on nested type constructors to a function on
-- composed type constructors
inCompose :: (f (g a) -> f (g b)) -> Compose f g a -> Compose f g b
inCompose f (Compose x) = Compose (f x)

-- | An implementation of gunfold with additional monadic context
gunfoldM :: (Data a,Monad m)
         => (forall b r. Data b => m (c (b -> r)) -> m (c r))
         -> (forall r. r -> m (c r)) -> Constr -> m (c a)
gunfoldM k z c = compose (gunfold (inCompose k) (Compose . z) c)

-- | An implementation of gunfold which additionally provides the
-- index for every child (zero-based).
gunfoldWithIndex :: Data a => (forall b r. Data b => Int -> c (b -> r) -> c r)
                 -> (forall r. r -> c r) -> Constr -> c a
gunfoldWithIndex k z c =
  evalState (gunfoldM applyWithIndex (return . z) c) 0
 where
  applyWithIndex cf = do
    f <- cf
    n <- get
    put (n+1)
    return (k n f)

-- | Build a term from a list of subterms and a generic function for
-- these subterms
fromConstrWithChildren  :: Data a => (forall b. Data b => c -> b) -> [c]
                        -> Constr -> a
fromConstrWithChildren toData children c =
  evalState (fromConstrM (fmap toData nextChild) c) children
 where
  nextChild = do
    xxs <- get
    case xxs of
         []   -> error "fromConstRec: Constr expects arguments but none \
                       \are provided"
         x:xs -> put xs >> return x
