{-# LANGUAGE DeriveDataTypeable #-}

module Test.Sloth.Function
  (
  isDataTypeFun,

  fun2, fun3, fun4, fun5, fun6, fun7
  ) where


import Data.Data ( Typeable,Data,DataType,dataTypeName )
import Data.List ( isPrefixOf )


isDataTypeFun :: DataType -> Bool
isDataTypeFun dt = "Test.Sloth.Function.Fun" `isPrefixOf` dataTypeName dt


data Fun2 a b = Fun2 a b
  deriving (Typeable,Data)

fun2 :: (a -> b -> c) -> Fun2 a b -> c
fun2 f (Fun2 x y) = f x y

data Fun3 a b c = Fun3 a b c
  deriving (Typeable,Data)

fun3 :: (a -> b -> c -> d) -> Fun3 a b c -> d
fun3 f (Fun3 x y z) = f x y z

data Fun4 a b c d = Fun4 a b c d
  deriving (Typeable,Data)

fun4 :: (a -> b -> c -> d -> e) -> Fun4 a b c d -> e
fun4 f (Fun4 x y z w) = f x y z w

data Fun5 a b c d e = Fun5 a b c d e
  deriving (Typeable,Data)

fun5 :: (a -> b -> c -> d -> e -> f) -> Fun5 a b c d e -> f
fun5 f (Fun5 x y z w u) = f x y z w u

data Fun6 a b c d e f = Fun6 a b c d e f
  deriving (Typeable,Data)

fun6 :: (a -> b -> c -> d -> e -> f -> g) -> Fun6 a b c d e f -> g
fun6 f (Fun6 x y z w u v) = f x y z w u v

data Fun7 a b c d e f g = Fun7 a b c d e f g
  deriving (Typeable,Data)

fun7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> Fun7 a b c d e f g -> h
fun7 f (Fun7 x y z w u v a) = f x y z w u v a
