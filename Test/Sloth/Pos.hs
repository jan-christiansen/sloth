{-# LANGUAGE DeriveDataTypeable #-}
-- | An implementation of positions which uniquely identify positions
--   in a term.
module Test.Sloth.Pos
  (
    Pos, isRoot, root, first, rest, (|>), singleton,

    bottom, PositionException(..),
    bottomNoPos, NoPositionException(..),

    mapWithPos
  ) where


import Data.List ( intersperse, isSuffixOf )
import Data.Data
import Control.Exception


infixl 5 |>

-- | Datatype to identify positions in a term
newtype Pos = Pos [Int]
  deriving (Eq,Data,Typeable)

instance Show Pos where
  showsPrec _ (Pos []) = showString "."
  showsPrec _ (Pos ps) = intercalateS "." (map shows (reverse ps))

intercalateS :: String -> [ShowS] -> ShowS
intercalateS x = foldr (.) id . intersperse (showString x)

instance Ord Pos where
  Pos p1 <= Pos p2 = p1 `isSuffixOf` p2
  Pos p1 >= Pos p2 = p2 `isSuffixOf` p1

-- | Test whether a postion is the root position
isRoot :: Pos -> Bool
isRoot (Pos l) = null l

-- | Root position
root :: Pos
root = Pos []

-- | First element of the position
first :: Pos -> Int
first (Pos ps) = last ps

-- | Position without the first element
rest :: Pos -> Pos
rest (Pos ps) = Pos (init ps)

-- | Extend a position with an element
(|>) :: Pos -> Int -> Pos
Pos ps |> p = Pos (p:ps)

-- | A position from a single integer
singleton :: Int -> Pos
singleton = (root |>)


data PositionException = PositionException Pos
  deriving (Typeable,Show)

instance Exception PositionException where
  toException = SomeException
  fromException (SomeException e) = cast e


data NoPositionException = NoPositionException
 deriving (Typeable,Show)

instance Exception NoPositionException where
  toException = SomeException
  fromException (SomeException e) = cast e


-- | An error with a given position
bottom :: Pos -> a
bottom p = throw (PositionException p)

-- | Used if a bottom value has no position information
bottomNoPos :: a
bottomNoPos = throw NoPositionException

-- | Map a function over a list and provide the function with the
-- correct position with respect to a root position
mapWithPos :: (Pos -> a -> b) -> Pos -> [a] -> [b]
mapWithPos f p = zipWith (\i x -> f (p|>i) x) [0..]
