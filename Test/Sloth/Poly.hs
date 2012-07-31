{-# LANGUAGE DeriveDataTypeable #-}
module Test.Sloth.Poly
  (
   A(..), isDataTypeA, polyPos, polyA
  ) where


import Data.Data
import Data.Maybe ( fromMaybe )

import Test.Sloth.Pos


-- | Data type used to check polymorphic functions. For example, to
-- check the function zip we annotate the type [A] -> [A] -> [(A, A)].
newtype A = A Pos
  deriving (Typeable,Data,Show)


-- | Check whether a DataType is the data type of A
isDataTypeA :: DataType -> Bool
isDataTypeA dt = dataTypeName dt == dataTypeName (dataTypeOf (undefined :: A))

-- | Construct a polymorphic A value with a specific position
polyA :: Data a => Pos -> a
polyA p = fromMaybe (error "polyA: cast error") (cast (A p))

-- | Select the position from a polymorphic A value
polyPos :: Data a => a -> Pos
polyPos x =
  case cast x of
       Just (A p) -> p
       Nothing    -> error "pos: cast error"
