module Test.Sloth.Search
  (
    SearchT(..), isCompleteT,

    Search(..), isComplete, search, completion, filterSearch, filterIncomplete,

    zero, one
  ) where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer

import Test.Sloth.CoMonad


-- | States whether we searched the complete search space or not
data Search a = Complete a
              | Incomplete a
  deriving Show


instance Applicative Search where
  pure = Complete

  (<*>) = ap

instance Functor Search where
  fmap f (Complete x)   = Complete (f x)
  fmap f (Incomplete x) = Incomplete (f x)

instance Monad Search where
  return = Complete

  Complete x >>= f = f x
  Incomplete x >>= f =
    case f x of
         Complete y -> Incomplete y
         r          -> r

instance CoPointed Search where
  extract (Incomplete x) = x
  extract (Complete   x) = x

instance CoMonad Search where
  duplicate (Complete x)   = Complete (Complete x)
  duplicate (Incomplete x) = Incomplete (Incomplete x)


-- | Case analysis for the Search data type
search :: (a -> b) -> (a -> b) -> Search a -> b
search f _ (Incomplete x) = f x
search _ f (Complete   x) = f x

-- | Check whether a search is complete
isComplete :: Search a -> Bool
isComplete (Complete _) = True
isComplete _            = False

-- | Replace an incomplete search by a complete one if the result
-- satisfies the predicate
completion :: (a -> Bool) -> Search a -> Search a
completion p = search (\x -> if p x then return x else Incomplete x) Complete

-- | Filter a list by a predicate that yields a search result. The
-- element is only droped if the predicate yields False and the search
-- was complete
filterSearch :: (a -> Search Bool) -> [a] -> [Search a]
filterSearch _ []     = []
filterSearch p (x:xs) =
  case p x of
       Complete   True  -> Complete x:ys
       Complete   False -> filterSearch p xs
       Incomplete _     -> Incomplete x:ys
 where
  ys = filterSearch p xs

-- | Filter the incomplete elements of a list by a predicate. We
-- assume that the list starts with incomplete elements
filterIncomplete :: (a -> Search Bool) -> [Search a] -> [Search a]
filterIncomplete p ars = cs ++ filterSearch p (map extract is)
 where
  (cs,is) = span isComplete ars


-- | Monad tansformer counterpart of Search
newtype SearchT m a = SearchT { runSearchT :: m (Search a) }

instance Functor f => Functor (SearchT f) where
  fmap f (SearchT m) = SearchT (fmap (fmap f) m)

instance MonadTrans SearchT where
  lift = SearchT . liftM Complete

instance Monad m => Applicative (SearchT m) where
  pure = return

  (<*>) = ap

instance Monad m => Monad (SearchT m) where
  return = lift . return

  m >>= f = SearchT (runSearchT m >>= g)
   where
    g (Complete   x) = runSearchT (f x)
    g (Incomplete x) = liftM (join . Incomplete) (runSearchT (f x))

instance CoPointed w => CoPointed (SearchT w) where
  extract = extract . extract . runSearchT

instance CoMonad w => CoMonad (SearchT w) where
  extend f m = SearchT (extend g (runSearchT m))
   where
    g mx = extend (\_ -> f (SearchT mx)) (extract (runSearchT m))


-- | Check whether a SearchT is complete
isCompleteT :: CoPointed w => SearchT w a -> Bool
isCompleteT (SearchT w) = isComplete (extract w)


-- | An incomplete Search with no results
zero :: a -> SearchT (Writer (Sum Int)) a
zero = SearchT . return . Incomplete

-- | A complete Search with one result
one :: a -> SearchT (Writer (Sum Int)) a
one = SearchT . (tell (Sum 1) >>) . return . Complete
